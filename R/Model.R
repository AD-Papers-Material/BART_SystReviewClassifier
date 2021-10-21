
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

# compute_pred_performance <- function(model, data = NULL, Y = NULL, summary = F,
# 																		 quants = getOption('baysren.probs'), AUC.thr = .9) {
#
# 	library(scales)
# 	library(pROC)
# 	library(glue)
# 	library(pbapply)
# 	library(pbmcapply)
#
# 	if (!is.null(data)) {
# 		if (is.null(Y)) stop('Define the Y variable name.')
#
# 		X <- data %>% select(all_of(model$X %>% colnames()))
# 		y <- data[[Y]]
# 	} else {
# 		X <- model$X
# 		y <- model$y
# 	}
#
# 	if (is.factor(y)) {
# 		y <- relevel(y, ref = levels(y)[2])
# 		y_levels <- levels(y)
# 	} else {
# 		y_levels <- sort(unique(y), T)
# 	}
#
# 	samples <- bart_machine_get_posterior(model, new_data = X)$y_hat_posterior_samples
#
# 	out <- mclapply(1:ncol(samples), function(i) {
# 		roc <- pROC::roc(response = y, predictor = samples[,i])
# 		data.frame(
# 			AUC = pROC::auc(roc) %>% as.vector(),
# 			pROC::coords(roc, "best",
# 									 ret = c('threshold', 'sensitivity',
# 									 				'specificity', 'accuracy', 'ppv', 'npv'),
# 									 transpose = F) %>%
# 				as.list() %>% as.data.frame.list() %>% head(1) %>%
# 				setNames(c('Threshold', 'Sens', 'Spec', 'Acc', 'PPV', 'NPV')),
# 			Sens.thr = quantile(samples[y == y_levels[1], i], min(quants)),
# 			Spec.thr = quantile(samples[y == y_levels[2], i], max(quants))
# 		)
# 	}) %>% bind_rows()
#
# 	if (summary) {
# 		summarise_pred_perf(out, quants)
# 	} else out
#
# }

# compute_pred_performance2 <- function(DTM, models, pred_quants = c(.01, .5, .99),
# 																			negLim = NULL, posLim = NULL) {
# 	message('Build indexes')
# 	index_data <- pblapply(1:length(models), function(i) {
# 		rbind(
# 			data.frame(
# 				IDs = models[[i]]$indexes$test$IDs,
# 				Sets = 'test',
# 				model_i = i
# 			),
# 			data.frame(
# 				IDs = models[[i]]$indexes$train$IDs,
# 				Sets = 'train',
# 				model_i = i
# 			)
# 		)
# 	}) %>% bind_rows()
#
# 	message('Aggregate predictions')
# 	aggr_preds <- lapply(c('test', 'train'), function(set) {
# 		IDs <- index_data %>% filter(Sets == set) %>% pull(IDs) %>% unique()
# 		pbmclapply(IDs, function(ID) {
#
# 			ID_preds <- index_data %>% filter(IDs == ID, Sets == set) %>% pull(model_i) %>%
# 				sapply(function(i) {
#
# 					models[[i]]$preds[DTM$ID %in% ID,]
# 				}) %>% t() %>% colMeans()
#
# 			data.frame(
# 				ID,
# 				Target = DTM$Target[DTM$ID == ID],
# 				ID_preds %>% quantile(pred_quants) %>% t %>%
# 					as.data.frame() %>%
# 					setNames(c('Pred_Med', 'Pred_Low', 'Pred_Up')),
# 				set = set,
# 				matrix(ID_preds, nrow = 1)
# 			)
# 		}) %>% bind_rows()
# 	}) %>% bind_rows()
#
# 	message('Compute performance')
# 	aggr_preds %>%
# 		mutate(
# 			Predicted_label = {
# 				if (is.null(negLim)) negLim <- max(Pred_Up[Target %in% 'n' & set == 'train'])
# 				if (is.null(posLim)) posLim <- min(Pred_Low[Target %in% 'y' & set == 'train'])
# 				case_when( # assign y or n if posterior lower/upper is outside the common range in the manually labeled articles, otherwise label as unknown
# 				Pred_Low > negLim & Pred_Low > posLim ~ 'y',
# 				Pred_Up < posLim & Pred_Up < negLim ~ 'n',
# 				T ~ 'unk'
# 			)
# 				},
# 			# Predicted_label = replace(
# 			# 	Predicted_label,
# 			# 	Predicted_label != Target & Predicted_label != 'unk',
# 			# 	'check'),
# 			.before = X1
# 		) %>%
# 		filter(!is.na(Target)) %>%
# 		group_by(set) %>%
# 		summarise(
# 			'AUC [CrI]' =  cur_data() %>% select(starts_with('X')) %>% pbmclapply(function(p) {
# 				suppressMessages(pROC::roc(response = Target, predictor = p) %>% pROC::auc() %>% as.vector())
# 			}) %>% unlist() %>% quantile(pred_quants) %>% percent() %>% {glue("{.[2]} [{.[1]}, {.[3]}]")},
# 			'SetLimits, neg/pos' = sprintf('%s/%s',
# 																		 min(Pred_Low[Target %in% 'y']) %>% percent,
# 																		 max(Pred_Up[Target %in% 'n']) %>% percent
# 			),
# 			'Predicted, n (%) [y/n]' = sapply(c('y', 'unk', 'n'), function(outc) {
# 				abs <- sum(Predicted_label == outc)
# 				perc <- percent(abs / n())
# 				pos <- sum(Target[Predicted_label == outc] == 'y')
# 				neg <- sum(Target[Predicted_label == outc] == 'n')
#
# 				glue('{outc}: {abs} ({perc}) [{pos}/{neg}]')
# 			}) %>% paste(collapse = ', '),
# 			'Observed, n (%)' = sapply(c('y', 'n'), function(outc) {
# 				abs <- sum(Target == outc)
# 				perc <- percent(abs / n())
# 				glue('{outc}: {abs} ({perc})')
# 			}) %>% paste(collapse = ', '),
# 			'Sens., pot. (act.)' = {
# 				x <- c(mean(Predicted_label[Target == 'y'] != 'n'), # potential
# 							 mean(Predicted_label[Target == 'y'] == 'y') # actual
# 				) %>% percent()
# 				glue('{x[1]} ({x[2]})')
# 			},
# 			'Spec., pot. (act.)' = {
# 				x <- c(mean(Predicted_label[Target == 'n'] != 'y'), # potential
# 							 mean(Predicted_label[Target == 'n'] == 'n') # actual
# 				) %>% percent()
# 				glue('{x[1]} ({x[2]})')
# 				},
# 			'PPV' = percent(mean(Target[Predicted_label == 'y'] == 'y')),
# 			'NPV' = percent(mean(Target[Predicted_label == 'n'] == 'n'))
# 		) %>%
# 		mutate(across(.fns = as.character)) %>%
# 		group_split(set) %>%
# 		lapply(function(df) tidyr::pivot_longer(df, everything(), names_to = 'Stat')) %>%
# 		{left_join(.[[2]], .[[1]], by = 'Stat')[-1, ]} %>%
# 		setNames(c('Statistic', 'Train', 'Test'))
# }

# compute_pred_performance <- function(data, samples = NULL, test_data = NULL,
# 																		 negLim = NULL, posLim = NULL,
# 																		 truePos = NULL, trueNeg = NULL,
# 																		 pred_quants = c(.01, .5, .99),
# 																		 show_progress = T) {
#
# 	if (show_progress) {
# 		iter_fun <- pblapply
# 	} else {
# 		iter_fun <- lapply
# 	}
#
# 	if ('Target' %nin% colnames(data)) {
# 		data$Target <- coalesce_labels(data)
# 	}
#
# 	if (!is.null(test_data)) {
# 		data <- import_classification(data, prev_records = test_data)
# 	}
#
# 	data <- data %>%
# 		select(any_of(c('ID', 'Target', 'Rev_previous', 'Pred_Med', 'Pred_Low',
# 										'Pred_Up', 'Predicted_label'))) %>%
# 		mutate(Set = ifelse(!is.na(Target), 'Train', 'Test'))
#
# 	obs <- coalesce_labels(data, c('Rev_previous', 'Target'))
# 	if (is.null(truePos)) truePos <- sum(obs %in% 'y', na.rm = T)
# 	if (is.null(trueNeg)) trueNeg <- sum(obs %in% 'n', na.rm = T)
# 	preds <- coalesce_labels(data, c('Rev_previous', 'Target',
# 																	 'Predicted_label'))
# 	predPos <- sum(preds %in% 'y', na.rm = T)
# 	predNeg <- sum(preds %in% 'n', na.rm = T)
# 	rm(obs, preds)
#
# 	if (!is.null(samples)) {
# 		if (nrow(data) != nrow(samples)) {
# 			stop('Data and posterior samples have a different number of rows')
# 		}
# 		data$Samples <- left_join(data[,'ID'], samples, by = "ID")
# 		#data$Samples <- samples[match(data$ID, samples$ID),]
# 	} else data$Samples <- data$Pred_Med
#
# 	iter_fun(list('Train', 'Test', c('Train', 'Test')), function(set) {
# 		data <- data %>%
# 			mutate(
# 				Target_test = coalesce_labels(cur_data(), c('Rev_previous', 'Target'))
# 			) %>%
# 			filter(Set %in% set)
#
# 		set <- paste(if (length(set) == 1) set else 'Total')
#
# 		if (nrow(data) == 0) return(data.frame(
# 			Value = c(glue('{set} (n = {nrow(data)})'), rep(NA, 9)) # Very ugly to put this number manually
# 		))
#
# 		data %>%
# 			filter(!is.na(Target_test)) %>%
# 			summarise(
# 				Set = glue('{set} (n = {n()})'),
#
# 				'AUC [CrI]' =  if (n_distinct(Target_test) == 1) {
# 					'One class only'
# 				} else {
# 					as.data.frame(cur_data()$Samples) %>%
# 						select(-any_of('ID')) %>%
# 						mclapply(function(p) {
#
# 							pROC::roc(response = Target_test, predictor = p) %>% #suppressMessages() %>%
# 								pROC::auc() %>% as.vector()
# 						}) %>% unlist() %>% {
# 							if (length(.) > 1) {
# 								quantile(., pred_quants) %>% percent() %>%
# 									{glue("{.[2]} [{.[1]}, {.[3]}]")}
# 							} else percent(.)
# 						}
# 				},
#
# 				'SetLimits, pos/neg' = if ('Train' %in% set) {
# 					if (is.null(negLim)) {
# 						negLim <- max(Pred_Up[Target %in% 'n'])
# 					}
#
# 					if (is.null(posLim)) {
# 						posLim <- min(Pred_Low[Target %in% 'y'])
# 					}
#
# 					c(posLim, negLim) %>%
# 						percent() %>% paste(collapse = '/')
# 				} else NA,
#
# 				'Predicted, n (%) [y/n]' = sapply(c('y', 'unk', 'check', 'n'), function(outc) {
# 					abs <- sum(data$Predicted_label == outc) # This include also non
# 					# labeled records, therefore it needs to be namespaced
#
# 					perc <- percent(abs / nrow(data))
# 					pos <- sum(Target_test[Predicted_label == outc] == 'y')
# 					neg <- sum(Target_test[Predicted_label == outc] == 'n')
#
# 					glue('{outc}: {abs} ({perc}) [{pos}/{neg}]')
# 				}) %>% paste(collapse = ', '),
#
# 				'Observed, n (%)' = summarise_vector(Target_test),
#
# 				'Sens., pot. (act.)' = {
# 					x <- c(mean(Predicted_label[Target_test == 'y'] != 'n'), # potential
# 								 mean(Predicted_label[Target_test == 'y'] == 'y') # actual
# 					) %>% percent()
# 					glue('{x[1]} ({x[2]})')
# 				},
#
# 				'Spec., pot. (act.)' = {
# 					x <- c(mean(Predicted_label[Target_test == 'n'] != 'y'), # potential
# 								 mean(Predicted_label[Target_test == 'n'] == 'n') # actual
# 					) %>% percent()
# 					glue('{x[1]} ({x[2]})')
# 				},
#
# 				# Samples %>% select(-ID) %>% pblapply(function(x) {
# 				# 	Predicted_label = case_when( # assign y if a record range is included into global y range and don't overlap the n range. The opposite is true for n labels
# 				# 		x > negLim & x > posLim ~ 'y',
# 				# 		x < posLim & x < negLim ~ 'n',
# 				# 		T ~ 'unk' # Assign unk if the label is not clearly defined
# 				# 	)
# 				#
# 				# 	Predicted_label = replace(
# 				# 		Predicted_label,
# 				# 		Predicted_label != Target & Predicted_label != 'unk',
# 				# 		'check')
# 				#
# 				# 	mean(Predicted_label[Target_test == 'n'] == 'n')
# 				# }) %>% unlist %>% quantile(pred_quants)
#
# 				PPV = percent(mean(
# 					Target_test[Predicted_label == 'y' |
# 												(Predicted_label == 'unk' & Target_test == 'y')] == 'y')),
# 				NPV = percent(mean(
# 					Target_test[Predicted_label == 'n' |
# 												(Predicted_label == 'unk' & Target_test == 'n')] == 'n')),
#
# 				## Similar results to the next indicators, but have finite bounds (no divisions by zero)
# 				# 'Pos per sample rate [CrI] (prob. > random)' = {
# 				# 	obsPos <- sum(Target_test %in% 'y')
# 				# 	obs <- (obsPos/qhyper(pred_quants, truePos, trueNeg, n())) %>%
# 				# 		signif(3)
# 				# 	obs_p <- phyper(obsPos, truePos, trueNeg, n()) %>% percent()
# 				#
# 				# 	pred <- (obsPos/qhyper(pred_quants, predPos, predNeg, n())) %>%
# 				# 		signif(3)
# 				# 	pred_p <- phyper(obsPos, predPos, predNeg, n()) %>% percent()
# 				#
# 				# 	glue('test: {obs[2]} [{obs[3]}, {obs[1]}] ({obs_p}), pred: {pred[2]} [{pred[3]}, {pred[1]}] ({pred_p})')
# 				# },
#
# 				'Random samples needed [CrI] (prob. < random)' = {
# 					qnhyper <- extraDistr::qnhyper
# 					pnhyper <- extraDistr::pnhyper
#
# 					obsPos <- sum(Target_test %in% 'y')
# 					obs <- (qnhyper(pred_quants, trueNeg, truePos, obsPos) / n()) %>%
# 						signif(3) %>% sort()
# 					obs_p <- (1 - pnhyper(n(), trueNeg, truePos, obsPos)) %>% percent()
#
# 					pred <- (qnhyper(pred_quants, predNeg, predPos, obsPos) / n()) %>%
# 						signif(3) %>% sort()
# 					pred_p <- (1 - pnhyper(n(), predNeg, predPos, obsPos)) %>% percent()
#
# 					glue('test: {obs[2]} [{obs[1]}, {obs[3]}] ({obs_p}), pred: {pred[2]} [{pred[1]}, {pred[3]}] ({pred_p})')
# 				},
#
# 				## https://doi.org/10.1186/s13643-016-0263-z
# 				## Not intuitive to explain
# 				# WWS = {
# 				# 	TN = sum(Predicted_label == 'n' & Target_test == 'n')
# 				# 	FN = sum(Predicted_label == 'n' & Target_test == 'y')
# 				# 	N = sum(!is.na(Target_test))
# 				# 	Sens = mean(Predicted_label[Target_test == 'y'] != 'n')
# 				#
# 				# 	(TN + FN) / N - (1 - Sens)
# 				# } %>% percent
# 			) %>%
# 			tidyr::pivot_longer(everything(), names_to = 'Statistic', values_to = set) %>% {
# 				if (!identical(set, 'Train')) .[,-1] else .
# 			}
# 	}) %>%
# 		bind_cols() #%>%
# 	#setNames(c('Statistic', 'Train', 'Test', 'Total'))
#
# }

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
				value = if (is.data.frame(obj)) paste(class(obj), collapse = ', ') else capture.output(str(obj)) %>% head() %>% paste(collapse = '\n') %>% str_trim()
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
		# bart.mods <- pblapply(1:n_models, function(i) {
		# 	all_data <- DTM %>% filter(!is.na(Target))
		#
		# 	if (resample) {
		# 		train_data <- slice_sample(all_data, prop = 1, replace = T) %>% {
		# 			.[c(rep(which(.$Target %in% 'y'), pos_mult), which(!(.$Target %in% 'y'))),]
		# 		}
		# 	} else train_data <- all_data
		#
		# 	#test_data <- all_data %>% filter(!(ID %in% train_data$ID))
		#
		# 	bart.mod <- compute_BART_model(train_data %>% select(-ID), 'Target',
		# 																 name = 'BartModel', rebuild = T, save = F,
		# 																 verbose = F, ...)
		#
		# 	list(
		#
		# 		preds = bart_machine_get_posterior(
		# 			bart.mod,
		# 			new_data = DTM %>% select(all_of(colnames(bart.mod$X)))
		# 		)$y_hat_posterior_samples,
		#
		# 		# indexes = list(
		# 		# 	train = train_data$ID %>% unique(),
		# 		# 	test = setdiff(all_data$ID, train_data$ID)
		# 		# ),
		# 		# oos.perf = compute_pred_performance(
		# 		# 	bart.mod, data = test_data, Y = 'Target', AUC.thr = AUC.thr,
		# 		# 	quants = pred_quants),
		#
		# 		var.imp = bartMachine::get_var_props_over_chain(bart.mod, 'trees')
		# 	)
		# })
		#
		#
		# message('Writing model to disk')
		# readr::write_rds(bart.mods, 'Model_backup.rds', compress = 'gz')
		# gc()

		Var_imp <- list()

		pb <- startpb(0, n_models)
		on.exit(closepb(pb))

		for (i in 1:n_models) {
			print(i)
			train_data <- DTM %>% filter(!is.na(Target))

			if (resample) {
				train_data <- slice_sample(train_data, prop = 1, replace = T) # %>% {
				# 	.[c(rep(which(.$Target %in% 'y'), pos_mult), which(!(.$Target %in% 'y'))),]
				# }
			}

			train_data <- train_data[c(rep(which(train_data$Target %in% 'y'), pos_mult),
																 which(!(train_data$Target %in% 'y'))),]

			bart.mod <- suppressMessages(compute_BART_model(train_data %>% select(-ID), 'Target',
																											name = 'BartModel', rebuild = T, save = F,
																											verbose = F, ...))
			rm(train_data)
			gc()

			message("predicting...")
			#pred_batch_size = 2 * 10^4
			#browser()
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

			#print(dim(preds))

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

	# Average posterior samples along the ensemble of models
	# Samples <- (Reduce(
	# 	"+",
	# 	bart.mods %>% lapply(`[[`, 'preds')
	# ) / length(bart.mods))

	Samples <- data.frame(ID = DTM$ID, Samples)

	# avg_preds <- bart.mods %>% pblapply(function(model) {
	# 	n.preds <- ncol(model$preds)
	# 	i <- sample(1:n.preds, round(n.preds/length(bart.mods)))
	# 	model$preds[,i]
	# }) %>% do.call(what = 'cbind')

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

		# if (!is.null(Test_data)) {
		#
		# 	tictoc::tic()
		#
		# 	Performance <- compute_pred_performance(Annotated_data, samples = Samples,
		# 																					test_data = Test_data,
		# 																					pred_quants = pred_quants)
		# 	tictoc::toc()
		# } else {
		# 	warning('compute_performance is TRUE but no test data')
		# }

		Performance <- estimate_performance(Annotated_data) %>%
			format_performance(session_names = session_name)

		print(Performance)
	}

	message('Adding variables\' importance')

	# var_imp <- lapply(1:n_models, function(i) {
	# 	data.frame(
	# 		Term = names(bart.mods[[i]]$var.imp),
	# 		Val = bart.mods[[i]]$var.imp
	# 	)
	# }) %>% bind_rows() %>%
	# 	group_by(Term) %>%
	# 	summarise(Value = mean(Val), Score = Value / sd(Val), n_models = n())

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

	# if ('Change: unlab. -> y' %in% names(iter_data)) {
	# 	last_iter_with_new_pos <- which(!is.na(iter_data[['Change: unlab. -> y']])) %>% max()
	#
	# 	if (nrow(iter_data) - last_iter_with_new_pos < stop_after) {
	#
	# 	}
	# }

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
		compute_changes(Annotated_data) #%>% select(matches('Change'))
	) %>%
		mutate(across(.fns = as.character)) %>%
		tidyr::pivot_longer(everything(), names_to = 'Indicator', values_to = 'Value')

	# Results <- data.frame(
	# 	Indicator = c(
	# 		'Parent file', 'Rerun n.', 'New labels', 'Records to review', 'Final labeling'
	# 	),
	# 	Value = c(
	# 		file,
	# 		cur_run,
	# 		Annotated_data$Rev_prediction_new %>%
	# 			summarise_vector(),
	# 		with(Annotated_data, Predicted_label[Rev_prediction_new %in% '*']) %>%
	# 			summarise_vector(),
	# 		coalesce_labels(Annotated_data, c('Rev_prediction_new','Rev_prediction',
	# 																			'Rev_manual', 'Predicted_label')) %>%
	# 			summarise_vector()
	# 	)
	# ) %>% bind_rows(
	# 	compute_changes(Annotated_data) %>%
	# 		select(matches('Change')) %>%
	# 		tidyr::pivot_longer(everything(), names_to = 'Indicator', values_to = 'Value') %>%
	# 		mutate(Value = as.character(Value))
	# )

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
