
coalesce_labels <- function(data, label_cols = c('Rev_prediction_new','Rev_prediction', 'Rev_manual')) {
	coalesce(!!!select(data, any_of(label_cols)))
}


create_training_set <- function(Records, min_freq = 0.05) {

	if (min_freq <= 0 | min_freq > 1) stop('"min_freq" should be between 0 and 1.')

	Records <- import_data(Records) %>%
		transmute(
			Target = coalesce_labels(.),
			ID, Title, Abstract, Authors, Keywords, Mesh
		)

	# Compute minimum term frequency among documents
	min_freq <- max(floor(sum(Records$Target %in% c('y', 'n')) * min_freq), 1)

	# Ensure that all positive document terms are kept
	Records <- Records[c(
		rep(which(Records$Target %in% 'y'), min_freq),
		which(Records$Target %nin% 'y')),]

	if (all(is.na(Records$Target))) {
		stop('There are no labelled records')
	}

	message('(min term freq in negatives: ', min_freq, ')')

	message('Title DTM')
	Title_DTM <- with(
		Records,
		text_to_DTM(Title, min.freq = min_freq, label = 'TITLE__', ids = ID,
								freq.subset.ids = ID[Target %in% c('y', 'n')])
	)

	message('dimensions: ', paste(dim(Title_DTM), collapse = ', '))

	message('\nAbstract DTM')
	Abstract_DTM <- with(
		Records,
		Abstract %>%
			str_remove_all(regex('\\b(background|introduction|method\\w*|result\\w*|conclusion\\w*|discussion)',ignore_case = T)) %>%
			text_to_DTM(min.freq = min_freq, label = 'ABSTR__', ids = ID,
									freq.subset.ids = ID[Target %in% c('y', 'n')])
	)

	message('dimensions: ', paste(dim(Abstract_DTM), collapse = ', '))

	message('\nAuthors DTM')
	Authors_DTM <- with(
		Records,
		text_to_DTM(Authors, tokenize.fun = tokenize_authors, min.freq = min_freq,
								label = 'AUTH__', ids = ID, freq.subset.ids = ID[Target %in% c('y', 'n')],
								add.ngrams = F, aggr.synonyms = F)
	)

	message('dimensions: ', paste(dim(Authors_DTM), collapse = ', '))

	message('\nKeywords DTM')
	Keywords_DTM <- with(
		Records,
		text_to_DTM(Keywords, tokenize.fun = tokenize_keywords, min.freq = min_freq,
								label = 'KEYS__', ids = ID,
								freq.subset.ids = ID[Target %in% c('y', 'n')])
	)

	message('dimensions: ', paste(dim(Keywords_DTM), collapse = ', '))

	message('\nMesh DTM')
	Mesh_DTM <- with(
		Records,
		text_to_DTM(Mesh, tokenize.fun = tokenize_MESH, min.freq = min_freq,
								label = 'MESH__', ids = ID, freq.subset.ids = ID[Target %in% c('y', 'n')],
								add.ngrams = F)
	)

	message('dimensions: ', paste(dim(Mesh_DTM), collapse = ', '))

	DTM <- data.frame(
		Target = Records$Target,
		Title_DTM,
		Abstract_DTM[,-1],
		Authors_DTM[,-1],
		Keywords_DTM[,-1],
		Mesh_DTM[,-1]
	) %>%
		distinct() %>% # remove the duplicated positive matches
		select(
			where(~ !is.numeric(.x)),
			where(~ suppressWarnings(sum(as.numeric(.x), na.rm = T)) > 1)
		)

}

compute_BART_model <- function(train_data, Y, preds = NULL, save = F,
															 folder = getwd(), name = as.character(Y),
															 rebuild = F, num_trees = 50, k = 2,
															 num_iterations_after_burn_in = 2000,
															 run_in_sample = F, mem_cache_for_speed = T,
															 use_missing_data = T, verbose = T, ...) {
	library(dplyr)
	library(bartMachine)
	library(glue)
	library(readr)

	if (!dir.exists(folder)) dir.create(folder, recursive = T)

	model_file <- file.path(folder, glue('{name}.rds'))

	if (is.null(preds)) preds <- colnames(train_data)
	preds <- setdiff(preds, Y)

	if (!file.exists(model_file) | rebuild) {

		train_data <- train_data %>%
			filter(across(all_of(Y), ~ !is.na(.x))) %>%
			mutate(across(all_of(Y), ~ if (!is.numeric(.x)) {
				if (is.factor(.x)) relevel(.x, ref = levels(.x)[2])
				else factor(.x) %>% relevel(ref = as.character(sort(unique(.x))[2]))
			} else .x))

		X <- train_data %>% select(all_of(preds)) %>% as.data.frame()
		Y <- train_data[[Y]]

		message('Building BART model...')
		model <- bartMachine(X = X, y = Y,
												 verbose = verbose,
												 num_trees = num_trees,
												 k = k,
												 num_iterations_after_burn_in = num_iterations_after_burn_in,
												 run_in_sample = run_in_sample,
												 mem_cache_for_speed = mem_cache_for_speed,
												 use_missing_data = use_missing_data, serialize = save, ...)

		if (save) {
			write_rds(model, model_file, compress = 'gz')
		}

	} else {
		message('Loading BART model...')
		model <- read_rds(model_file)

	}

	model
}

enrich_annotation_file <- function(session_name, file = NULL, DTM = NULL,
																	 ## Model parameters
																	 pos_mult = 10,
																	 n_models = 10,
																	 resample = F,
																	 pred_quants = c(.01, .5, .99),
																	 #
																	 sessions_folder = getOption("baysren.sessions_folder"),
																	 pred_batch_size = 5000,
																	 autorun = TRUE, replication = NULL,
																	 stop_on_unreviewed = TRUE,
																	 dup_session_action = c('fill', 'add',
																	 											 'replace', 'stop'),
																	 limits = list(
																	 	stop_after = 4, pos_target = NULL,
																	 	labeling_limit = NULL
																	 ),
																	 compute_performance = FALSE,
																	 test_data = NULL,
																	 use_prev_labels = TRUE,
																	 prev_classification = NULL,
																	 save_samples = TRUE,
																	 rebuild = FALSE, ...) {

	# pick the last annotated record file or the source one if any
	if (is.null(file)) {
		file <- get_session_files(session_name, sessions_folder)$Annotations %>%
			last()

		if (is.null(file)) {
			stop('No annotation files in this session, or the session folder doesn\'t exists.')
		}
		message('Processing file: ', file)
	}

	process_id <- paste0('.pID__', str_replace_all(file, fixed(.Platform$file.sep), '__'))

	if (file.exists(process_id)) {
		message('File already being processed. Skipping.')
		return(NULL)
	}

	writeLines(paste('Process started on:', safe_now()), process_id)

	on.exit(file.remove(process_id))

	if (length(pred_quants) != 3 & any(pred_quants >= 1 | pred_quants <= 0)) {
		stop('"pred_quants" should be 3 quantiles, between 0 and 1 (included).')
	}

	# log arguments. useful for error checking
	Arguments <- match.call() %>%
		as.list() %>% names() %>% lapply(function(name) {

			if (name == '' || !exists(name)) return(NULL)
			obj <- get(name)
			data.frame(
				name,
				value = if (is.data.frame(obj)) {
					paste(class(obj), collapse = ', ')
					} else {
						capture.output(str(obj)) %>%
							head() %>%
							paste(collapse = '\n') %>%
							str_trim()
					}
			)
		}) %>% bind_rows()

	pred_quants <- sort(pred_quants)[c(2, 1, 3)]

	dup_session_action <- match.arg(dup_session_action)

	dup_session_action <- if (dup_session_action == 'fill') 'silent' else dup_session_action

	session_path <- create_session(Records = file, session_name = session_name,
																 sessions_folder = sessions_folder,
																 dup_session_action = dup_session_action)

	if (pos_mult < 1) stop('"pos_mult" should be at least 1.')

	if (pos_mult != round(pos_mult)) stop('"pos_mult" should be an integer value.')

	message('Loading Annotation file')

	tictoc::tic()
	# Read the file and use (theoretically) all rows to infer the column type, to
	# avoid misspecification errors.
	Records <- read_excel(file, guess_max = 10^6)

	if (all(is.na(Records$Rev_manual))) {
		stop('No manually labeled entries found. This may also happen if there is a great number of missings before the first labeled record.')
	}

	repl <- 1

	# If the file is an annotated file the replication is extracted
	if (basename(dirname(file)) == 'Annotations') {

		# replication is extracted from the file title or set to one
		prev_run <- str_extract(file, '(?<=_rep)\\d+') %>% as.numeric()

		repl <- max(1, prev_run, na.rm = T)

		# increase the replication if no new positives
		iter_data <- compute_changes(Records) %>%
			select(-matches('unlab\\. -> unlab\\.|y -> y|n -> n'))

		new_positives <- iter_data %>% select(matches('-> y')) %>% rowSums()

		if (new_positives == 0) repl <- repl + 1 else repl <- 1
	}

	if (!is.null(limits) & !isFALSE(limits)) {

		message('Checking conditions for a new iteration')

		Target <- coalesce_labels(Records) %>% na.omit()

		# All records have been labeled
		if (length(Target) == nrow(Records)) {
			message('No unlabeled records left!')

			return(NULL)
		}

		# Enough positives found
		if (!is.null(limits$pos_target) &&
				sum(Target %in% 'y') >= limits$pos_target) {
			message('Positive records target reached!')
			return(NULL)
		}

		# else if (!is.null(autorun$review_limit)) {
		# 	n_reviews <- Annotated_data$Rev_prediction_new %>% na.omit() %>% length()
		# 	if ((autorun$review_limit <= 1 & n_reviews / nrow(Annotated_data) >=  autorun$review_limit) |
		# 			(autorun$review_limit > 1 & n_reviews >=  autorun$review_limit))
		# 		message('Num/Ratio of labeled records above threshold!')
		# 	return(NULL)
		# }

		# Too many records have been labeled
		if (!is.null(limits$labeling_limit)) {
			if ((limits$labeling_limit <= 1 & length(Target) / nrow(Records) >= limits$labeling_limit) |
					(limits$labeling_limit > 1 & length(Target) >= limits$labeling_limit)) {
				message('Num/Ratio of labeled records above threshold!')
				return(NULL)
			}
		}

		if (!is.null(limits$stop_after) && (repl > limits$stop_after)) {
			message('Reached limit of consecutive iterations without new positive records!')
			return(NULL)
		}

		rm(Target)
	}

	# Add Rev_prediction for storing prediction reviews if missing
	if ('Rev_prediction' %nin% names(Records)) {
		Records <- Records %>%
			mutate(Rev_prediction = NA, .after = Rev_manual)
	}

	if ('*' %in% Records$Rev_prediction_new & stop_on_unreviewed) {
		stop('There are unreviewed predictions.')
	} else {
		Records$Rev_prediction_new <- replace(Records$Rev_prediction_new,
																					Records$Rev_prediction_new == '*',
																					NA)
	}

	# Coalesce Rev_prediction_new into Rev_prediction and clear the former
	Records <- Records %>%
		mutate(
			Rev_prediction = coalesce_labels(., c('Rev_prediction_new', 'Rev_prediction')),
			Rev_prediction_new = NA,
			.after = Rev_prediction
		)
	tictoc::toc()

	if (!is.null(prev_classification)) {
		message('Importing previous classification')

		tictoc::tic()

		Records <- import_classification(
			records = Records,
			prev_records = import_data(prev_classification)
		)

		tictoc::toc()
	}

	if (!is.null(test_data)) {
		message('Importing test data')

		tictoc::tic()
		Test_data <- import_data(test_data)

		tictoc::toc()
	} else Test_data <- NULL

	tictoc::tic()

	if (is.null(DTM) & repl > 1) {
		DTM <- file.path(sessions_folder, session_name, 'DTM.rds')
	}

	# Reload DTM if existing and there were no new positive matches
	if (is.character(DTM) && file.exists(DTM)) {
		message('Loading DTM')

		DTM <- read_rds(DTM)

	} else {
		message('Creating DTM')

		DTM <- create_training_set(Records)

		message('Saving DTM')

		##
		DTM_file <- file.path(session_path, 'DTM.rds')

		readr::write_rds(DTM, file = DTM_file, compress = 'gz')
		##
	}

	if (!(all(Records$ID %in% DTM$ID))) {
		stop('The DTM and the records should be compatible (same IDs).')
	}

	# Import the labeling from the reviewed data to the DTM
	Cur_Target <- Records %>%
		transmute(
			ID,
			Target = coalesce_labels(.)
		)

	if (any(Cur_Target$Target %nin% c(NA, 'y', 'n'))) {
		stop('Labels can only be "y" or "n"')
	}

	DTM$Target <- NULL

	DTM <- left_join(DTM, Cur_Target, by = 'ID')

	# Add features reporting the number of terms present in each block
	for (field in c('ABSTR', 'TITLE', 'KEYS', 'MESH')) {
		DTM[[paste0(field, '.count')]] <- select(DTM, contains(field)) %>%
			rowSums(na.rm = T)
	}
	tictoc::toc()

	message('Training data:')
	message('Positives: ', sum(DTM$Target == 'y', na.rm = T))
	message('Negatives: ', sum(DTM$Target == 'n', na.rm = T))
	message('Features: ', select(DTM, -ID, -Target) %>% ncol)

	message(glue('\nModel generation (repl: {repl})'))

	if (rebuild == FALSE & file.exists('Model_backup.rds')) {
		message('(loading from disk...)')
		gc()
		bart.mods <- readr::read_rds('Model_backup.rds')

		Samples <- bart.mods$Samples
		Var_imp <- bart.mods$Var_imp

	}
	else {

		Var_imp <- list()

		pb <- startpb(0, n_models)
		on.exit(closepb(pb))

		for (i in 1:n_models) {
			print(i)
			train_data <- DTM %>% filter(!is.na(Target))

			if (resample) {
				train_data <- slice_sample(train_data, prop = 1, replace = T)
			}

			train_data <- train_data[c(rep(which(train_data$Target %in% 'y'), pos_mult),
																 which(!(train_data$Target %in% 'y'))),]

			bart.mod <- suppressMessages(compute_BART_model(train_data %>% select(-ID), 'Target',
																											name = 'BartModel', rebuild = T, save = F,
																											verbose = F, ...))
			rm(train_data)
			gc()

			message("predicting...")

			preds <- pblapply(0:floor(nrow(DTM) / pred_batch_size), function (i) {
				gc()
				start <- i * pred_batch_size + 1
				stop <- min((i + 1) * pred_batch_size, nrow(DTM))
				idx <- start:stop

				bart_machine_get_posterior(
					bart.mod,
					new_data = DTM[idx,] %>% select(all_of(colnames(bart.mod$X)))
				)$y_hat_posterior_samples

			}) %>% do.call(what = 'rbind')

			if (!exists('Samples')) {
				Samples <- preds
			} else {
				Samples <- Samples + preds
			}

			Var_imp[[i]] <- bartMachine::get_var_props_over_chain(bart.mod, 'trees')

			rm(bart.mod, preds)

			gc()

			setpb(pb, i)
		}

		# Average posterior samples along the ensemble of models
		Samples <- Samples / n_models


		message('\nWriting model to disk')
		readr::write_rds(list(Samples = Samples, Var_imp = Var_imp),
										 'Model_backup.rds', compress = 'gz')
		gc()
	}

	DTM <- DTM %>% select(-matches('\\.count$'))

	message('Generating labels')

	tictoc::tic()

	Samples <- data.frame(ID = DTM$ID, Samples)

	Predicted_data <- DTM %>% select(ID, Target) %>%
		data.frame(
			apply(Samples[,-1], 1, quantile, pred_quants) %>% t %>%
				as.data.frame() %>%
				setNames(c('Pred_Med', 'Pred_Low', 'Pred_Up'))
		) %>%
		mutate(
			Pred_delta = Pred_Up - Pred_Low, # add posterior interval range
			Predicted_label = {
				negLim <- max(Pred_Up[Target %in% 'n'])
				posLim <- min(Pred_Low[Target %in% 'y'])

				case_when( # assign y if a record range is included into global y range and don't overlap the n range. The opposite is true for n labels
					Pred_Low > negLim & Pred_Low > posLim ~ 'y',
					Pred_Up < posLim & Pred_Up < negLim ~ 'n',
					T ~ 'unk' # Assign unk if the label is not clearly defined
				)
			},
			Predicted_label = replace(
				Predicted_label,
				Predicted_label != Target & Predicted_label != 'unk',
				'check'), # mark if predicted label is in contrast with the training data
			across(matches('Pred_'), signif, 3)
		)

	Annotated_data <- left_join(
		select(Records, -any_of(colnames(Predicted_data %>% select(-ID)))),
		Predicted_data,
		by = 'ID'
	) %>%
		select(Order, matches('^Rev'), Predicted_label, matches('^Pred'), everything()) %>%
		arrange(Order) %>%
		mutate(
			Rev_prediction_new = {
				if (!use_prev_labels | 'Rev_previous' %nin% names(.)) {
					Previous_lab <- rep('*', n())
				} else {
					Previous_lab <- replace(Rev_previous, is.na(Rev_previous), '*')
				}

				case_when(
					!is.na(Rev_prediction) | Predicted_label == 'n' ~ NA_character_,
					Predicted_label == Rev_manual ~ NA_character_,
					Predicted_label %in%  c('check', 'unk', 'y') ~ Previous_lab
				)
			}
		)

	tictoc::toc()

	Performance <- NULL

	if (compute_performance) {
		message('Adding performance summary (may take a while...)')

		Performance <- estimate_performance(Annotated_data) %>%
			format_performance(session_names = session_name)

		print(Performance)
	}

	message('Adding variables\' importance')

	Var_imp <- lapply(1:n_models, function(i) {
		data.frame(
			Term = names(Var_imp[[i]]),
			Val = Var_imp[[i]]
		)
	}) %>% bind_rows() %>%
		group_by(Term) %>%
		summarise(
			Value = mean(Val),
			Score = if (n_models > 1) Value / sd(Val) else Value,
			n_models = n()
		)

	message('Adding annotation summary')

	Results <- tibble(
		Iter = (list.files(file.path(session_path, 'Annotations'), pattern = '.xlsx') %>%
							str_subset('~\\$', negate = T) %>% length()) + 1,
		'Parent file' = file,
		'Replication n.' = repl,
		'N. features' = select(DTM, -ID, -Target) %>% ncol,
		'New labels' = Annotated_data$Rev_prediction_new %>%
			summarise_vector(),
		'Records to review' = with(Annotated_data, Predicted_label[Rev_prediction_new %in% '*']) %>%
			summarise_vector(),
		'Final labeling' = coalesce_labels(Annotated_data, c('Rev_prediction_new','Rev_prediction',
																												 'Rev_manual', 'Predicted_label')) %>%
			summarise_vector()
	) %>% bind_cols(
		compute_changes(Annotated_data)
	) %>%
		mutate(across(.fns = as.character)) %>%
		tidyr::pivot_longer(everything(), names_to = 'Indicator', values_to = 'Value')

	print(as.data.frame(Results))

	message('Exporting')

	out <- list(
		Annotated_data = Annotated_data,
		Results = Results,
		Performance = Performance,
		Variable_importance = Var_imp,
		Arguments = Arguments
	) %>% Filter(f = Negate(is.null))

	common_tag <- glue('{if (repl > 1) paste0("rep", repl, "_") else ""}{safe_now()}')
	iter <- with(Results, Value[Indicator == 'Iter'])

	message('- annotated records...')
	tictoc::tic()
	output_file_ann <- file.path(session_path, 'Annotations',
															 glue('{iter}.Records_{common_tag}.xlsx'))

	dir.create(dirname(output_file_ann), showWarnings = F, recursive = T)

	openxlsx::write.xlsx(out, file = output_file_ann, asTable = T)

	tictoc::toc()

	message('- annotation summary...')
	tictoc::tic()
	output_file_res <- file.path(session_path, 'Results',
															 glue('{iter}.Results_{common_tag}.csv'))
	dir.create(dirname(output_file_res), showWarnings = F, recursive = T)

	write_csv(Results, file = output_file_res)

	tictoc::toc()

	if (save_samples) {
		message('- posterior samples...')
		tictoc::tic()

		output_file_samp <- file.path(session_path, 'Samples',
																	glue('{iter}.Samples_{common_tag}.rds'))
		dir.create(dirname(output_file_samp), showWarnings = F, recursive = T)

		readr::write_rds(Samples, file = output_file_samp, compress = 'gz')

		tictoc::toc()
	}

	if (file.exists('Model_backup.rds')) file.remove('Model_backup.rds')

	file.remove(process_id)

	if (autorun) {
		if ('*' %nin% Annotated_data$Rev_prediction_new) {
			message('\n\nAutomatic restart')

			rm(Predicted_data, Annotated_data, Var_imp, Samples)
			gc()

			enrich_annotation_file(output_file_ann,
														 DTM = get_session_files(session_name, sessions_folder)$DTM,
														 ## Model parameters
														 pos_mult = pos_mult,
														 n_models = n_models,
														 pred_quants = pred_quants,
														 resample = resample,
														 #
														 dup_session_action = 'fill', # if an automatic reply, this is the logical option
														 session_name = session_name,
														 replication = NULL, # the replication number will be taken by the previous annotation
														 compute_performance = compute_performance,
														 sessions_folder = sessions_folder, limits = limits,
														 autorun = autorun, use_prev_labels = use_prev_labels,
														 prev_classification = NULL, # if present it was already imported
														 test_data = test_data,
														 rebuild = TRUE, # redundant since the model backup was removed, but just to be on the safe side
														 ...)
		} else {
			message('Manual labeling needed!')
		}
	}

	invisible(out)
}

consolidate_results <- function(session_name, sessions_folder = getOption("baysren.sessions_folder")) {
	annotations_files <- get_session_files(session_name, sessions_folder)$Annotations

	message('Loading annotations...')
	annotations <- annotations_files %>%
		pbmclapply(function(file) {
			import_data(file)
		})

	message('Consolidating results...')
	pbmclapply(1:length(results_files), function(i) {
		final_results <- annotations[[i]] %>%
			compute_changes() %>%
			mutate_all(as.character) %>%
			tidyr::pivot_longer(
				cols = everything(),
				names_to = 'Indicator',
				values_to = 'Value'
			)

		annotations_files[[i]] %>%
			import_data(sheet = 'Results') %>%
			filter(Indicator %nin% c(
				final_results$Indicator, "New labels", "Records to review",
				"Final labeling")) %>%
			filter(!str_detect(Indicator, '\\*')) %>%
			bind_rows(final_results) %>%
			bind_rows(
				data.frame(
					Indicator = 'Reviewed predictions',
					Value = summarise_vector(annotations[[i]]$Rev_prediction_new)
				)
			) %>%
			write_csv(results_files[[i]])
	}) %>% invisible()
}


perform_grid_evaluation <- function(records, sessions_folder = 'Grid_Search',
																		prev_classification = records,
																		## Model parameters
																		resample = c(FALSE, TRUE),
																		n_init = c(50, 100, 250, 500),
																		n_models = c(1, 5, 10, 20, 40, 60),
																		pos_mult = c(1, 10, 20),
																		pred_quants = list(c(.1, .5, .9),
																											 c(.05, .5, .95),
																											 c(.01, .5, .99)),
																		## Passed arguments
																		rebuild = TRUE,
																		limits = list(
																			stop_after = 4,
																			pos_target = NULL, labeling_limit = NULL
																		)) {

	# file: 'Grid_Search/Classification_data.xlsx'

	# prepare the parameter grid
	Grid <- tidyr::expand_grid(
		resample, n_init, n_models, pos_mult, pred_quants
	) %>%
		mutate(
			iteration = glue('{1:n()} / {n()}'),
			session = paste(
				'GridSession',
				glue('{n_models}Mods'),
				glue('{ifelse(resample, "y", "n")}Resamp'),
				glue('{n_init}Init'),
				glue('{pos_mult}Mult'),
				glue('{(sapply(pred_quants, max)-sapply(pred_quants, min))*100}Quant'),
				sep = '.'
			) %>% str_replace_all('\\.+', '.')
		)

	Records <- import_data(records)
	Classification_data <- import_data(prev_classification)

	# import the test classifications and remove unclassified records
	Records <- Records %>%
		import_classification(Classification_data) %>%
		filter(!is.na(Rev_previous))

	pblapply(1:nrow(Grid), function(i) {
		str(Grid[i,], give.attr = FALSE)

		if (file.exists('Model_backup.rds')) file.remove('Model_backup.rds')

		session_path <- file.path(sessions_folder, Grid[i,]$session)

		# if no record files are present, recreate the session folder
		if (is.null(get_session_files(Grid[i,]$session, sessions_folder)$Records)) {
			Records <- Records %>%
				# remove labels in excess of "n_init"
				mutate(
					Rev_manual = coalesce_labels(Records, c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous')) %>%
						replace(Order > Grid[i,]$n_init, NA),
					Rev_prediction = NA,
					Rev_prediction_new = NA
					)

			create_session(Records, session_name = Grid[i,]$session,
										 sessions_folder = sessions_folder,
										 dup_session_action = 'replace')
		}

		# pick the last annotated record file or the source one if any
		last_record_file <- get_session_files(Grid[i,]$session, sessions_folder)$Annotations %>%
			last()

		if (is.null(last_record_file)) {
			last_record_file <- get_session_files(Grid[i,]$session, sessions_folder)$Records
		}

		with(Grid[i,],
				 enrich_annotation_file(last_record_file, session_name = session,
				 											 sessions_folder = sessions_folder, rebuild = TRUE,
				 											 prev_classification = Classification_data, DTM = NULL,
				 											 use_prev_labels = TRUE,
				 											 autorun = TRUE, replication = NULL,
				 											 compute_performance = FALSE,
				 											 test_data = NULL,
				 											 dup_session_action = 'fill',
				 											 limits = limits,
				 											 ## Model parameters
				 											 resample = resample,
				 											 n_models = n_models,
				 											 pos_mult = pos_mult,
				 											 pred_quants = pred_quants[[1]])
		)
	}) %>% unlist() %>% table()
}
