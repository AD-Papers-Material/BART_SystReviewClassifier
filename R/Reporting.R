summarise_by_source <- function(annotation_file, as_data_frame = FALSE,
																add_totals = TRUE) {
	data <- import_data(annotation_file)

	sources <- data$Source %>% str_split(., '; *') %>% unlist() %>% unique

	total_records <- nrow(data)

	res <- lapply(sources, function(source) {
		Records <- str_detect(data$Source, glue('{source}')) %>% sum
		Perc_over_total <- percent(Records/total_records)
		Source_specific <- str_detect(data$Source, glue('^{source}$')) %>% sum
		Source_specific_perc <- percent(Source_specific / Records)

		list(Records = Records, Perc_over_total = Perc_over_total,
				 Source_specific = Source_specific, Source_specific_perc = Source_specific_perc)
	}) %>% setNames(sources)

	if (add_totals) {
		res$Total <- list(
			Records = nrow(data),
			Perc_over_total = '',
			Source_specific = NA,
			Source_specific_perc = ''
		)
	}

	if (as_data_frame) {
		res <- res %>% lapply(as.data.frame.list) %>% bind_rows() %>%
			mutate(
				Source = names(res),
				.before = 1
			) %>%
			arrange(desc(Records))
	}

	res
}

summarise_sources_by_sessions <- function(sessions, sessions_folder = 'Sessions',
																					add_totals = TRUE, keep_session_label = TRUE) {
	if (length(sessions) == 1) {
		res <- get_session_last_files(file.path(sessions_folder, session))$Records %>%
			summarise_by_source(as_data_frame = TRUE)

		return(res)
	}

	records <- pbmclapply(sessions, function(session) {
		get_session_last_files(file.path(sessions_folder, session))$Records %>%
			import_data()
	}) %>% setNames(sessions)

	res <- mclapply(1:length(records), function(i) {
		data <- records[[i]]

		if (i > 1) {
			previous_records <- bind_rows(records[1:(i - 1)])$ID
			data <- data %>% filter(ID %nin% previous_records)
		}

		summarise_by_source(data, as_data_frame = TRUE) %>%
			mutate(
				Session_label = sessions[i]
			)
	})

	if (add_totals) {
	res <- bind_rows(
			res,
			summarise_by_source(last(records), as_data_frame = TRUE) %>%
				mutate(
					Session_label = 'All Sessions'
				)
		)
	}

	res <- res %>%
		group_by(Session_label) %>%
		mutate(
			Session = c(Session_label[1], rep('', n() - 1)),
			.before = 1
		) %>% ungroup()

	if (!keep_session_label) {
		res$Session_label <- NULL
	}

	res
}

get_source_distribution <- function(annotation_file, as_propr, format_fun = percent) {
	res <- import_data(annotation_file)$Source %>%
		pbmclapply(function(sources) str_split(sources, '; *') %>% unlist %>% n_distinct) %>%
		unlist() %>% table()

	if (as_propr) {
		res <- prop.table(res) %>% format_fun()
	}

	res
}


summarise_annotations <- function(annotation.folder = 'Annotations', plot = F) {
	# list.files('Models') %>% pbmclapply(function(file) {
	# 	Model <- read_rds(file.path('Models', file))

	Res <- list.files(annotation.folder, recursive = T, pattern = 'Records_P') %>%
		str_subset('~\\$', negate = T) %>%
		pbmclapply(function(file) {

			file_path <- file.path(annotation.folder, file)

			Annotated_data <- read_excel(file_path)

			Performance <- read_excel(file_path, sheet = 'Out_of_sample_perf')
			Performance <- as.list(Performance$Value) %>% setNames(Performance$Indicator)

			Var_imp <- read_excel(file_path, sheet = 'Variable_importance') %>%
				filter(!str_detect(Term, 'MESH|KEYS')) %>%
				mutate(Term = str_remove(Term, '.+__')) %>%
				group_by(Term) %>% slice_max(order_by = Score) %>% ungroup() %>%
				arrange(desc(Score)) %>% pull(Term) %>% head(15) %>%
				str_replace('^\\w', str_to_upper) %>%
				str_replace_all(c('\\._\\.' = ' ', '\\.' = ' OR ', '_' = ' ')) %>%
				paste(collapse =', ')

			Predicted <- Annotated_data$Predicted_label
			# Manual <- with(Annotated_data, case_when(
			# 	!is.na(Rev_prediction) ~ as.character(Rev_prediction),
			# 	!is.na(Rev_abstract) ~ Rev_abstract,
			# 	T ~ Rev_title
			# ))
			Manual <- coalesce_labels(Annotated_data, c('Rev_prediction', 'Rev_manual'))

			status <- c(
				Uncertain = sum(Predicted %in% 'unk'),
				Positive = sum(Predicted %in% 'y'),
				Negative = sum(Predicted %in% 'n'),
				New_positive = sum(Predicted %in% 'y' & is.na(Manual)),
				New_negative = sum(Predicted %in% 'n' & is.na(Manual)),
				New_uncertain = with(Annotated_data, sum(Predicted_label == 'unk' & is.na(Rev_prediction))),
				Reviewed_positive = sum(Manual %in% 'y'),
				Reviewed_negative = sum(Manual %in% 'n'),
				Reviewed = sum(!is.na(Manual)),
				Discordant = sum(Predicted == 'check'),
				False_positive = sum(Predicted %in% 'y' & Manual %in% 'n'),
				AUC = Performance$AUC,
				Sensitivity = Performance$Sens,
				Specificity = Performance$Spec,
				Important_vars = Var_imp
			)

			data.frame(
				Value = status,
				Label = names(status),
				Total_records = nrow(Annotated_data),
				Date = str_remove_all(basename(file), 'Records_P_(R_)?|.xlsx') %>% as_datetime(),
				File = basename(file),
				Batch = dirname(file),
				Parent_file = ifelse(!is.null(Annotated_data$Parent_file), basename(Annotated_data$Parent_file[1]), NA)
			)
		}) %>% bind_rows() %>% arrange(Date) %>%
		mutate(
			Ord = factor(paste(Date, str_detect(File, '_R_')), levels = unique(paste(Date, str_detect(File, '_R_')))) %>% as.numeric
		) %>%
		arrange(Ord)

	if (plot) {

		# p <- Res %>%
		# 	mutate(
		# 		Parent_ord = sapply(Parent_file, function(par_file) {
		# 			res <- Ord[File == par_file] %>% unique()
		#
		# 			if (length(res) == 0) res <- NA
		#
		# 			res
		# 		}),
		# 		Rev_parent = sapply(1:n(), function(i) {
		# 			d <- Date[i]
		# 			ifelse(str_detect(File[i], '_R_') & !all(str_detect(File[Date %in% d], '_R_')), Ord[Date %in% d][1], NA)
		# 		})
		# 	) %>%
		# 	ggplot(aes(x = Ord)) +
		# 	geom_col(aes(fill = factor(Label) %>% relevel('Reviewed'), y = Value), width = .75, position = 'stack') +
		# 	geom_line(aes(y = Total_positives, linetype = 'Positives'), color = 'red') +
		# 	geom_point(aes(y = Total_positives, size = factor(Total_records)), color = 'red') +
		# 	geom_label(aes(y = Total_positives, label = Total_positives), nudge_y = 5) +
		# 	geom_label(aes(y = 0, label = paste0(
		# 		ifelse(str_detect(File, '_R_'), 'R', 'P'),
		# 		ifelse(!is.na(Rev_parent), paste0('(', Rev_parent, ')'), ''),
		# 		ifelse(!is.na(Parent_ord), paste0(':', Parent_ord), '')
		# 		)), nudge_y = 5) +
		# 	geom_errorbarh(
		# 		data = Res %>%
		# 			group_by(Batch) %>%
		# 			summarise(first = min(Ord), last = max(Ord), val = max(Value) * 1.05) %>%
		# 			mutate(Ord = 1:n()),
		# 		mapping = aes(xmin = first, xmax = last, y = val),
		# 		height = 1
		# 	) +
		# 	geom_text(
		# 		data = Res %>%
		# 			group_by(Batch) %>%
		# 			summarise(x = median(Ord), y = max(Value) * 1.1, text = paste('Query:', str_remove_all(Batch, 'Records_') %>% ymd())) %>%
		# 			mutate(Ord = 1:n()),
		# 		mapping = aes(x = x, y = y, label = text)
		# 	) +
		# 	scale_x_continuous(breaks = Res$Ord %>% unique) +
		# 	theme_minimal() +
		# 	labs(size = 'Total Records', x = 'Annotation sequence', y = 'Records', fill = 'Record type', linetype = NULL)

		Plot_data <- Res %>%
			mutate(
				Parent_ord = sapply(Parent_file, function(par_file) {
					res <- Ord[File == par_file] %>% unique()

					if (length(res) == 0) res <- NA

					res
				}),
				Rev_parent = sapply(1:n(), function(i) {
					d <- Date[i]
					ifelse(str_detect(File[i], '_R_') & !all(str_detect(File[Date %in% d], '_R_')), Ord[Date %in% d][1], NA)
				}),
				Batch = Batch %>% str_remove('Records_'),
				Ord_lab = case_when(
					str_detect(File, '_R_') ~ sprintf('R%d (P%d)', Ord, Rev_parent),
					T ~ sprintf('P%d: R%d', Ord, Parent_ord)
				) %>% str_remove(': RNA')
			) %>% ungroup()


		Totals <- Plot_data %>% select(Batch, Total_records, Ord) %>% group_by(Batch) %>% slice_head(n = 1) %>% ungroup()

		Predictions <- Plot_data %>% filter(Label %in% c('Uncertain', 'Positive', 'Discordant') & !str_detect(File, '_R_'))

		Reviews_new <- Plot_data %>% filter(Label %in% c('New positive', 'New uncertain') & !str_detect(File, '_R_'))
		Reviews_tot <- Plot_data %>% filter(Label %in% c('Total positive', 'Reviewed', 'False positive') & str_detect(File, '_R_'))

		p_totals <- ggplot(Totals, aes(Batch, Total_records)) +
			geom_col(fill = 'steelblue', width = .25) +
			geom_label(aes(label = format(Total_records, big.mark = ' '))) +
			labs(x = 'Query date', y = 'Records') +
			theme_minimal() +
			ggtitle('A) Total records by query')

		p_review_new <- ggplot(Reviews_new, aes(reorder(Ord_lab, Ord), Value, group = Label)) +
			geom_col(aes(fill = Label), Predictions, position = position_dodge2(padding = .5), alpha = .8) +
			geom_line(aes(color = Label), size = 1) +
			geom_point(aes(color = Label)) +
			geom_label(aes(label = Value, color = Label), data = filter(Reviews_new, Value > 0), fontface = "bold", show.legend = F) +
			labs(x = 'Task', y = 'Records', color = 'New results', fill = 'Predicted label') +
			facet_wrap(~ paste('Query:', Batch), scales = 'free_x') +
			scale_color_manual(values = c('#d95f02', '#7b3294')) +
			theme_minimal() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			ggtitle('B) Prediction tasks')

		p_review_tot <- ggplot(Reviews_tot, aes(reorder(Ord_lab, Ord), Value, group = Label)) +
			geom_line(aes(color = Label), size = 1) +
			geom_point(aes(color = Label)) +
			geom_label_repel(aes(label = Value, color = Label), data = filter(Reviews_tot, Value > 0), force = 0.01, force_pull = 20, fontface = "bold", show.legend = F) +
			scale_color_manual(values = c('#ca0020', '#0571b0', '#008837')) +
			labs(x = 'Task', y = 'Records', color = 'Review results') +
			theme_minimal() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			ggtitle('C) Review tasks')

		p_review_tot <- Reviews_tot %>%
			group_by(Batch) %>%
			summarise(
				med = Ord_lab[Ord == median(Ord)],
				first = Ord_lab[which.min(Ord)],
				last = Ord_lab[which.max(Ord)],
				y = max(Value)) %>%
			with(
				p_review_tot +
					annotate('errorbarh', xmin = first, xmax = last, y = max(y) * 1.05, height = 1) +
					annotate('text', x = med, y = max(y) * 1.1, label = paste("Query:", Batch))
			)


		p <- p_totals | (p_review_new / p_review_tot)

		print(p)
	}

	Res
}

summarise_annotations2 <- function(sessions_folder = 'Sessions',
																	 sessions = list.dirs(sessions_folder, recursive = F),
																	 analyse_iterations = T, analyse_perf = T,
																	 ...) {

	# Just to be sure
	sessions <- file.path(sessions_folder, basename(sessions))

	output <- list()

	files <- lapply(sessions, function(session_folder) {
		annotation_files <- list.files(file.path(session_folder, 'Annotations'),
																	 pattern = '.xlsx', full.names = T) %>%
			str_subset('~\\$', negate = T)

		results_files <- list.files(file.path(session_folder, 'Results'),
																pattern = '.csv', full.names = T) %>%
			str_subset('~\\$', negate = T)

		samples_files <- list.files(file.path(session_folder, 'Samples'),
																pattern = '.rds', full.names = T)

		output <- lapply(0:length(annotation_files), function(i) {
			if (i == 0) {
				files <- c(Annotation = (list.files(session_folder, pattern = '.xlsx',
																						full.names = T) %>%
																 	str_subset('~\\$', negate = T))[1])
			} else {
				files <- c(Annotation = annotation_files[i], Samples = samples_files[i],
									 Results = results_files[i])
			}

			data.frame(
				Session = basename(session_folder),
				Iter = if (i == 0) 0 else str_extract(basename(files[1]), '^\\d+') %>%
					as.numeric(),
				Type = names(files),
				File = files
			)
		}) %>% bind_rows()

	}) %>% bind_rows() %>%
		tidyr::pivot_wider(id_cols = c(Session, Iter), names_from = Type,
											 values_from = File) %>%
		arrange(Session, Iter)

	message('...loading files')

	Annotations <- pbmclapply(files$Annotation, import_data) %>%
		setNames(files$Annotation)

	Parent_files <- pbmclapply(na.omit(files$Results), function(f) {
		df <- read_csv(f, col_types = cols())
		df$Value[df$Indicator == 'Parent file']
	}) %>% setNames(na.omit(files$Results))

	if (analyse_perf) {
		Samples <- pbmclapply(na.omit(files$Samples), read_rds) %>%
			setNames(na.omit(files$Samples))

		message('...compute performance data')

		output$Performance = pblapply(names(Samples), function(file) {

			files <- files %>% filter(Samples == file)

			output$Performance = compute_pred_performance(
				Annotations[[files$Annotation]], samples = Samples[[file]],
				show_progress = F, ...
			) %>%
				mutate(
					Session = files$Session,
					Iter = files$Iter,
					File = basename(files$Annotation),
					.before = 1
				)

		}) %>% bind_rows()
	}

	# TODO: use the Results files if existing and fix Iters_w_no_pos
	if (analyse_iterations) {
		message('...compute iteration data')

		output$Iterations = lapply(names(Annotations), function(file) {

			files <- files %>% filter(Annotation == file)

			Annotations[[file]] %>%
				compute_changes() %>%
				mutate(
					Session = files$Session,
					Iter = files$Iter,
					File = basename(file),
					Parent_file = if (!is.na(files$Results)) {
						Parent_files[[files$Results]] %>% basename()
					} else NA
				) %>%
				select(-matches('unlab\\. -> unlab\\.|y -> y|n -> n')) %>%
				mutate(
					Iters_w_no_pos = {
						pos_vec <- select(., matches('-> y')) %>% rowSums(na.rm = T)
						out <- rep(0, length(pos_vec))

						for (i in 2:length(pos_vec)) {
							if (pos_vec[i] > 0) out[i] <- 0 else out[i] <- out[i - 1] + 1
						}

						out
					},
					across(where(is.numeric), ~ replace(.x, 1:length(.x) != 1 & is.na(.x), 0))
				)
		}) %>% bind_rows() %>%
			select(
				Session, Iter, File, Parent_file,
				matches('Target'), Total_labeled, New_labels, Iters_w_no_pos, matches('Change')
			)
	}

	output
}


plot_predicted_pos_rate <- function(Ann_data, block_size = 50) {

	# plot_predicted_pos_rate(a %>% mutate(Rev_previous = coalesce_labels(., c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous')) %>% replace(Order > 1200, NA)), block_size = 50)

	Ann_data <- Ann_data %>%
		transmute(
			Order,
			Train = Rev_previous == 'y',
			Target = coalesce_labels(., c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous')) == 'y'
		) %>%
		group_by(Block = ceiling(1:n()/block_size)) %>%
		summarise(
			Order = last(Order),
			block_size = n(),
			Train = sum(Train),
			Target = sum(Target, na.rm = T)
		)
	#
	# mod <- brm(Train | trials(block_size) ~ Order, family = binomial, data = Ann_data,
	# 					 cores = 8, refresh = 0, iter = 8000,
	# 					 backend = 'cmdstan',
	# 					 prior = c(prior(student_t(1, 0, 5), class = 'Intercept'),
	# 					 					prior(student_t(1, 0, 2.5), class = 'b')))

	mod <- brms::brm(bf(Train | trials(block_size) ~ a * Order ^ b, a + b ~ 1, nl = T), family = binomial, data = Ann_data,
									 cores = 8, refresh = 0, iter = 8000,
									 backend = 'cmdstan',
									 prior = c(set_prior('student_t(1, 0, 5)', nlpar = "a"),
									 					set_prior('student_t(1, 0, 2.5)', nlpar = "b")))

	## To estimate the total number of positives and from there the positives in a
	## random subsample
	# posterior_predict(mod, newdata = data.frame( block_size,
	# Order = seq(1200, nrow(a), block_size) )) %>% rowSums() %>% quantile(c(.01,
	# .05, .5, .95, .99)) %>% {qhyper(p = .95, (96+ .)/nrow(a) * 1200, 1200 * (1 -
	# (96+ .)/nrow(a)), 500)} print(mod)

	posterior_predict(mod, newdata = data.frame(
		block_size,
		Order = seq(block_size, limit, block_size)
	)) %>% rowSums() %>% quantile(seq(0,1, .1)) %>%
		print()

	limit <- min(Ann_data$Order[last(which(Ann_data$Target > 0))], last(Ann_data$Order))

	train_limit = max(Ann_data$Order[!is.na(Ann_data$Train)])

	data.frame(
		block_size,
		Order = seq(block_size, limit, block_size)
	) %>%
		left_join(Ann_data) %>%
		mutate(
			Target_lab = Target,
			Target = Target / block_size,
			(posterior_predict(mod, newdata = cur_data()) / block_size) %>%
				apply(2, quantile, c(.05, .5, .95)) %>% t %>%
				as.data.frame() %>%
				setNames(c('Low', 'Med', 'Up')),
			across(c(Target_lab, Target), ~ replace(.x, .x == 0, NA))
		) %>%
		ggplot(aes(Order)) +
		geom_ribbon(aes(ymin = Low, ymax = Up), alpha = .25) +
		geom_vline(xintercept = train_limit, linetype = 'dashed', alpha = .5) +
		geom_segment(aes(xend = Order, y = 0, yend = Target, color = 'Obs.'), size = 1) +
		geom_line(aes(y = Med, color = 'Pred.'), size = 1, linetype = 'dashed', alpha = .8) +
		geom_label(aes(y = Target, label = Target_lab)) +
		theme_minimal() +
		scale_x_continuous(breaks = seq(block_size, limit, block_size)) +
		scale_y_continuous(trans = 'log1p', breaks = seq(0, .3, .01)) +
		labs(y = 'Pos. rate', x = 'Records', color = NULL) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	# ggplot(Ann_data, aes(Order)) +
	# 	#geom_line(aes(y = Pred_Up, color = 'Up')) +
	# 	#geom_line(aes(y = Pred_Low, color = 'Low')) +
	# 	geom_errorbar(aes(ymin = Pred_Low, ymax = Pred_Up, color = coalesce_labels(a)), width = 0) +
	# 	geom_point(aes(y = Pred_Med, color = Predicted_label), alpha = .8) +
	# 	scale_y_continuous(trans = 'logit') + theme_minimal()
}
