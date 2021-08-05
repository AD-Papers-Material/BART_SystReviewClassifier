summarise_by_source <- function(annotation_file, as_data_frame = FALSE,
																add_session_totals = TRUE) {
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

	if (add_session_totals) {
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
			arrange(desc(Records)) %>%
			setNames(c('Source', 'Records', '% over total', 'Source specific records', '% over source total'))
	}

	res
}

summarise_sources_by_session <- function(sessions = list.files(sessions_folder),
																				 sessions_folder = options("basren.sessions_folder")[[1]],
																				 add_global_totals = TRUE, keep_session_label = FALSE, ...) {
	if (length(sessions) == 1) {
		res <- get_session_files(session, sessions_folder)$Records %>%
			summarise_by_source(as_data_frame = TRUE, ...)

		return(res)
	}

	records <- pbmclapply(sessions, function(session) {
		get_session_files(session, sessions_folder)$Records %>%
			import_data()
	}) %>% setNames(sessions)

	res <- mclapply(1:length(records), function(i) {
		data <- records[[i]]

		if (i > 1) {
			previous_records <- bind_rows(records[1:(i - 1)])$ID
			data <- data %>% filter(ID %nin% previous_records)
		}

		summarise_by_source(data, as_data_frame = TRUE, ...) %>%
			mutate(
				Session_label = sessions[i]
			)
	})

	if (add_global_totals) {
		res <- bind_rows(
			res,
			summarise_by_source(last(records), as_data_frame = TRUE, ...) %>%
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

source_session_summary_to_list <- function(source_summary) {
	source_summary$Session_label %>% unique() %>%
		lapply(function(session) {
			df <- source_summary %>% filter(Session_label == session)
			df$Source %>%
				lapply(function(source) {
					df <- source_summary %>% filter(Session_label == session, Source == source)
					list(
						Records = df$Records,
						Perc_over_total = df$`% over total`,
						Source_specific = df$`Source specific records`,
						Source_specific_perc = df$`% over source total`
					)
				}) %>% setNames(df$Source)
		}) %>% setNames(unique(source_summary$Session_label))
}

get_source_distribution <- function(annotation_file, as_propr = TRUE, format_fun = percent) {
	res <- import_data(annotation_file)$Source %>%
		pbmclapply(function(sources) str_split(sources, '; *') %>% unlist %>% n_distinct) %>%
		unlist() %>% table()

	if (as_propr) {
		res <- prop.table(res) %>% format_fun()
	}

	res
}

summarise_annotations <- function(session_name, sessions_folder = options("basren.sessions_folder")[[1]],
																	remove_empty_columns = TRUE, remove_raw_data = TRUE) {
	result_list <- get_session_files(session_name, sessions_folder)$Results %>%
		lapply(function(file) {
			file %>%
				import_data() %>%
				mutate(
					Value = suppressWarnings(as.numeric(Value))
				) %>%
				tidyr::pivot_wider(everything(), names_from = Indicator, values_from = Value)
		})

	total_records <- result_list[[1]] %>% select(matches('Change:')) %>% rowSums()

	lapply(0:length(result_list), function(i) {

		template <- tibble(
			'Change: unlab. -> y' = 0,
			'Change: unlab. -> n' = 0,
			'Change: unlab. -> *' = 0,
			'Change: y -> n' = 0,
			'Change: n -> y' = 0
		)


		if (i == 0) {
			result_data <- result_list[[1]] %>%
				select(!any_of(colnames(template))) %>% # Remove all changes info
				mutate(
					Iter = 'Initial labelling',
					'Target: y' = result_list[[1]] %>% select(matches('Change: y')) %>%
						rowSums(),
					'Target: n' = result_list[[1]] %>% select(matches('Change: n')) %>%
						rowSums(),
					'Change: unlab. -> y' = `Target: y`,
					'Change: unlab. -> n' = `Target: n`,
				)
		} else {
			result_data <-  result_list[[i]]
		}

		result_data %>%
			bind_cols(
				template %>% select(!any_of(colnames(result_data)))
			) %>%
			transmute(
				Iteration = as.character(Iter),
				Positives = `Target: y`,
				Negatives = `Target: n`,
				tot_reviewed_ = Positives + Negatives,
				total_records_ = total_records,
				'Total labelled (%)' = glue('{tot_reviewed_} ({percent(tot_reviewed_ / total_records)})'),
				'Unlab. -> y' = `Change: unlab. -> y`,
				'Unlab. -> n' = `Change: unlab. -> n`,
				'Unlab. -> *' = `Change: unlab. -> *`,
				'y -> n' = `Change: y -> n`,
				'n -> y' = `Change: n -> y`,
			) %>%
			mutate(
				Changes = select(., matches('->')) %>% rowSums()
			)
	}) %>% bind_rows() %>%  {
		if (remove_empty_columns) {
			. <- select(., where(~ any(.x > 0)))
		}

		if (remove_raw_data) {
			. <- select(., !matches('_$'))
		}

		.
	}
}

summarise_annotations_by_session <- function(sessions_folder = options("basren.sessions_folder")[[1]],
																						 remove_empty_columns = TRUE,
																						 remove_raw_data = TRUE) {
	sessions <- list.files(sessions_folder)

	if (length(sessions) == 0) {
		stop('No session found in "', sessions_folder, '". Are you sure the name is not mispelled?')
	}

	mclapply(1:length(sessions), function(i) {
		session <- sessions[i]

		res <- summarise_annotations(session, sessions_folder,
																 remove_empty_columns = F, remove_raw_data = F)

		if (i > 1) {
			res <- tail(res, -1)
		}

		res %>%
			mutate(
				Session = c(glue("{session} (n = {res$total_records_[1]})"), rep('', nrow(res) - 1)),
				Session_ = session,
				.before = 1
			)
	}) %>% bind_rows() %>%  {
		if (remove_empty_columns) {
			. <- select(., where(~ any(.x > 0)))
		}

		if (remove_raw_data) {
			. <- select(., !matches('_$'))
		}

		.
	}
}

	# summarise_annotations <- function(annotation.folder = 'Annotations', plot = F) {
	# 	# list.files('Models') %>% pbmclapply(function(file) {
	# 	# 	Model <- read_rds(file.path('Models', file))
	#
	# 	Res <- list.files(annotation.folder, recursive = T, pattern = 'Records_P') %>%
	# 		str_subset('~\\$', negate = T) %>%
	# 		pbmclapply(function(file) {
	#
	# 			file_path <- file.path(annotation.folder, file)
	#
# 			Annotated_data <- read_excel(file_path)
#
# 			Performance <- read_excel(file_path, sheet = 'Out_of_sample_perf')
# 			Performance <- as.list(Performance$Value) %>% setNames(Performance$Indicator)
#
# 			Var_imp <- read_excel(file_path, sheet = 'Variable_importance') %>%
# 				filter(!str_detect(Term, 'MESH|KEYS')) %>%
# 				mutate(Term = str_remove(Term, '.+__')) %>%
# 				group_by(Term) %>% slice_max(order_by = Score) %>% ungroup() %>%
# 				arrange(desc(Score)) %>% pull(Term) %>% head(15) %>%
# 				str_replace('^\\w', str_to_upper) %>%
# 				str_replace_all(c('\\._\\.' = ' ', '\\.' = ' OR ', '_' = ' ')) %>%
# 				paste(collapse =', ')
#
# 			Predicted <- Annotated_data$Predicted_label
# 			# Manual <- with(Annotated_data, case_when(
# 			# 	!is.na(Rev_prediction) ~ as.character(Rev_prediction),
# 			# 	!is.na(Rev_abstract) ~ Rev_abstract,
# 			# 	T ~ Rev_title
# 			# ))
# 			Manual <- coalesce_labels(Annotated_data, c('Rev_prediction', 'Rev_manual'))
#
# 			status <- c(
# 				Uncertain = sum(Predicted %in% 'unk'),
# 				Positive = sum(Predicted %in% 'y'),
# 				Negative = sum(Predicted %in% 'n'),
# 				New_positive = sum(Predicted %in% 'y' & is.na(Manual)),
# 				New_negative = sum(Predicted %in% 'n' & is.na(Manual)),
# 				New_uncertain = with(Annotated_data, sum(Predicted_label == 'unk' & is.na(Rev_prediction))),
# 				Reviewed_positive = sum(Manual %in% 'y'),
# 				Reviewed_negative = sum(Manual %in% 'n'),
# 				Reviewed = sum(!is.na(Manual)),
# 				Discordant = sum(Predicted == 'check'),
# 				False_positive = sum(Predicted %in% 'y' & Manual %in% 'n'),
# 				AUC = Performance$AUC,
# 				Sensitivity = Performance$Sens,
# 				Specificity = Performance$Spec,
# 				Important_vars = Var_imp
# 			)
#
# 			data.frame(
# 				Value = status,
# 				Label = names(status),
# 				Total_records = nrow(Annotated_data),
# 				Date = str_remove_all(basename(file), 'Records_P_(R_)?|.xlsx') %>% as_datetime(),
# 				File = basename(file),
# 				Batch = dirname(file),
# 				Parent_file = ifelse(!is.null(Annotated_data$Parent_file), basename(Annotated_data$Parent_file[1]), NA)
# 			)
# 		}) %>% bind_rows() %>% arrange(Date) %>%
# 		mutate(
# 			Ord = factor(paste(Date, str_detect(File, '_R_')), levels = unique(paste(Date, str_detect(File, '_R_')))) %>% as.numeric
# 		) %>%
# 		arrange(Ord)
#
# 	if (plot) {
#
# 		# p <- Res %>%
# 		# 	mutate(
# 		# 		Parent_ord = sapply(Parent_file, function(par_file) {
# 		# 			res <- Ord[File == par_file] %>% unique()
# 		#
# 		# 			if (length(res) == 0) res <- NA
# 		#
# 		# 			res
# 		# 		}),
# 		# 		Rev_parent = sapply(1:n(), function(i) {
# 		# 			d <- Date[i]
# 		# 			ifelse(str_detect(File[i], '_R_') & !all(str_detect(File[Date %in% d], '_R_')), Ord[Date %in% d][1], NA)
# 		# 		})
# 		# 	) %>%
# 		# 	ggplot(aes(x = Ord)) +
# 		# 	geom_col(aes(fill = factor(Label) %>% relevel('Reviewed'), y = Value), width = .75, position = 'stack') +
# 		# 	geom_line(aes(y = Total_positives, linetype = 'Positives'), color = 'red') +
# 		# 	geom_point(aes(y = Total_positives, size = factor(Total_records)), color = 'red') +
# 		# 	geom_label(aes(y = Total_positives, label = Total_positives), nudge_y = 5) +
# 		# 	geom_label(aes(y = 0, label = paste0(
# 		# 		ifelse(str_detect(File, '_R_'), 'R', 'P'),
# 		# 		ifelse(!is.na(Rev_parent), paste0('(', Rev_parent, ')'), ''),
# 		# 		ifelse(!is.na(Parent_ord), paste0(':', Parent_ord), '')
# 		# 		)), nudge_y = 5) +
# 		# 	geom_errorbarh(
# 		# 		data = Res %>%
# 		# 			group_by(Batch) %>%
# 		# 			summarise(first = min(Ord), last = max(Ord), val = max(Value) * 1.05) %>%
# 		# 			mutate(Ord = 1:n()),
# 		# 		mapping = aes(xmin = first, xmax = last, y = val),
# 		# 		height = 1
# 		# 	) +
# 		# 	geom_text(
# 		# 		data = Res %>%
# 		# 			group_by(Batch) %>%
# 		# 			summarise(x = median(Ord), y = max(Value) * 1.1, text = paste('Query:', str_remove_all(Batch, 'Records_') %>% ymd())) %>%
# 		# 			mutate(Ord = 1:n()),
# 		# 		mapping = aes(x = x, y = y, label = text)
# 		# 	) +
# 		# 	scale_x_continuous(breaks = Res$Ord %>% unique) +
# 		# 	theme_minimal() +
# 		# 	labs(size = 'Total Records', x = 'Annotation sequence', y = 'Records', fill = 'Record type', linetype = NULL)
#
# 		Plot_data <- Res %>%
# 			mutate(
# 				Parent_ord = sapply(Parent_file, function(par_file) {
# 					res <- Ord[File == par_file] %>% unique()
#
# 					if (length(res) == 0) res <- NA
#
# 					res
# 				}),
# 				Rev_parent = sapply(1:n(), function(i) {
# 					d <- Date[i]
# 					ifelse(str_detect(File[i], '_R_') & !all(str_detect(File[Date %in% d], '_R_')), Ord[Date %in% d][1], NA)
# 				}),
# 				Batch = Batch %>% str_remove('Records_'),
# 				Ord_lab = case_when(
# 					str_detect(File, '_R_') ~ sprintf('R%d (P%d)', Ord, Rev_parent),
# 					T ~ sprintf('P%d: R%d', Ord, Parent_ord)
# 				) %>% str_remove(': RNA')
# 			) %>% ungroup()
#
#
# 		Totals <- Plot_data %>% select(Batch, Total_records, Ord) %>% group_by(Batch) %>% slice_head(n = 1) %>% ungroup()
#
# 		Predictions <- Plot_data %>% filter(Label %in% c('Uncertain', 'Positive', 'Discordant') & !str_detect(File, '_R_'))
#
# 		Reviews_new <- Plot_data %>% filter(Label %in% c('New positive', 'New uncertain') & !str_detect(File, '_R_'))
# 		Reviews_tot <- Plot_data %>% filter(Label %in% c('Total positive', 'Reviewed', 'False positive') & str_detect(File, '_R_'))
#
# 		p_totals <- ggplot(Totals, aes(Batch, Total_records)) +
# 			geom_col(fill = 'steelblue', width = .25) +
# 			geom_label(aes(label = format(Total_records, big.mark = ' '))) +
# 			labs(x = 'Query date', y = 'Records') +
# 			theme_minimal() +
# 			ggtitle('A) Total records by query')
#
# 		p_review_new <- ggplot(Reviews_new, aes(reorder(Ord_lab, Ord), Value, group = Label)) +
# 			geom_col(aes(fill = Label), Predictions, position = position_dodge2(padding = .5), alpha = .8) +
# 			geom_line(aes(color = Label), size = 1) +
# 			geom_point(aes(color = Label)) +
# 			geom_label(aes(label = Value, color = Label), data = filter(Reviews_new, Value > 0), fontface = "bold", show.legend = F) +
# 			labs(x = 'Task', y = 'Records', color = 'New results', fill = 'Predicted label') +
# 			facet_wrap(~ paste('Query:', Batch), scales = 'free_x') +
# 			scale_color_manual(values = c('#d95f02', '#7b3294')) +
# 			theme_minimal() +
# 			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# 			ggtitle('B) Prediction tasks')
#
# 		p_review_tot <- ggplot(Reviews_tot, aes(reorder(Ord_lab, Ord), Value, group = Label)) +
# 			geom_line(aes(color = Label), size = 1) +
# 			geom_point(aes(color = Label)) +
# 			geom_label_repel(aes(label = Value, color = Label), data = filter(Reviews_tot, Value > 0), force = 0.01, force_pull = 20, fontface = "bold", show.legend = F) +
# 			scale_color_manual(values = c('#ca0020', '#0571b0', '#008837')) +
# 			labs(x = 'Task', y = 'Records', color = 'Review results') +
# 			theme_minimal() +
# 			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# 			ggtitle('C) Review tasks')
#
# 		p_review_tot <- Reviews_tot %>%
# 			group_by(Batch) %>%
# 			summarise(
# 				med = Ord_lab[Ord == median(Ord)],
# 				first = Ord_lab[which.min(Ord)],
# 				last = Ord_lab[which.max(Ord)],
# 				y = max(Value)) %>%
# 			with(
# 				p_review_tot +
# 					annotate('errorbarh', xmin = first, xmax = last, y = max(y) * 1.05, height = 1) +
# 					annotate('text', x = med, y = max(y) * 1.1, label = paste("Query:", Batch))
# 			)
#
#
# 		p <- p_totals | (p_review_new / p_review_tot)
#
# 		print(p)
# 	}
#
# 	Res
# }
#
# summarise_annotations2 <- function(sessions_folder = 'Sessions',
# 																	 sessions = list.dirs(sessions_folder, recursive = F),
# 																	 analyse_iterations = T, analyse_perf = T,
# 																	 ...) {
#
# 	# Just to be sure
# 	sessions <- file.path(sessions_folder, basename(sessions))
#
# 	output <- list()
#
# 	files <- lapply(sessions, function(session_folder) {
# 		annotation_files <- list.files(file.path(session_folder, 'Annotations'),
# 																	 pattern = '.xlsx', full.names = T) %>%
# 			str_subset('~\\$', negate = T)
#
# 		results_files <- list.files(file.path(session_folder, 'Results'),
# 																pattern = '.csv', full.names = T) %>%
# 			str_subset('~\\$', negate = T)
#
# 		samples_files <- list.files(file.path(session_folder, 'Samples'),
# 																pattern = '.rds', full.names = T)
#
# 		output <- lapply(0:length(annotation_files), function(i) {
# 			if (i == 0) {
# 				files <- c(Annotation = (list.files(session_folder, pattern = '.xlsx',
# 																						full.names = T) %>%
# 																 	str_subset('~\\$', negate = T))[1])
# 			} else {
# 				files <- c(Annotation = annotation_files[i], Samples = samples_files[i],
# 									 Results = results_files[i])
# 			}
#
# 			data.frame(
# 				Session = basename(session_folder),
# 				Iter = if (i == 0) 0 else str_extract(basename(files[1]), '^\\d+') %>%
# 					as.numeric(),
# 				Type = names(files),
# 				File = files
# 			)
# 		}) %>% bind_rows()
#
# 	}) %>% bind_rows() %>%
# 		tidyr::pivot_wider(id_cols = c(Session, Iter), names_from = Type,
# 											 values_from = File) %>%
# 		arrange(Session, Iter)
#
# 	message('...loading files')
#
# 	Annotations <- pbmclapply(files$Annotation, import_data) %>%
# 		setNames(files$Annotation)
#
# 	Parent_files <- pbmclapply(na.omit(files$Results), function(f) {
# 		df <- read_csv(f, col_types = cols())
# 		df$Value[df$Indicator == 'Parent file']
# 	}) %>% setNames(na.omit(files$Results))
#
# 	if (analyse_perf) {
# 		Samples <- pbmclapply(na.omit(files$Samples), read_rds) %>%
# 			setNames(na.omit(files$Samples))
#
# 		message('...compute performance data')
#
# 		output$Performance = pblapply(names(Samples), function(file) {
#
# 			files <- files %>% filter(Samples == file)
#
# 			output$Performance = compute_pred_performance(
# 				Annotations[[files$Annotation]], samples = Samples[[file]],
# 				show_progress = F, ...
# 			) %>%
# 				mutate(
# 					Session = files$Session,
# 					Iter = files$Iter,
# 					File = basename(files$Annotation),
# 					.before = 1
# 				)
#
# 		}) %>% bind_rows()
# 	}
#
# 	# TODO: use the Results files if existing and fix Iters_w_no_pos
# 	if (analyse_iterations) {
# 		message('...compute iteration data')
#
# 		output$Iterations = lapply(names(Annotations), function(file) {
#
# 			files <- files %>% filter(Annotation == file)
#
# 			Annotations[[file]] %>%
# 				compute_changes() %>%
# 				mutate(
# 					Session = files$Session,
# 					Iter = files$Iter,
# 					File = basename(file),
# 					Parent_file = if (!is.na(files$Results)) {
# 						Parent_files[[files$Results]] %>% basename()
# 					} else NA
# 				) %>%
# 				select(-matches('unlab\\. -> unlab\\.|y -> y|n -> n')) %>%
# 				mutate(
# 					Iters_w_no_pos = {
# 						pos_vec <- select(., matches('-> y')) %>% rowSums(na.rm = T)
# 						out <- rep(0, length(pos_vec))
#
# 						for (i in 2:length(pos_vec)) {
# 							if (pos_vec[i] > 0) out[i] <- 0 else out[i] <- out[i - 1] + 1
# 						}
#
# 						out
# 					},
# 					across(where(is.numeric), ~ replace(.x, 1:length(.x) != 1 & is.na(.x), 0))
# 				)
# 		}) %>% bind_rows() %>%
# 			select(
# 				Session, Iter, File, Parent_file,
# 				matches('Target'), Total_labeled, New_labels, Iters_w_no_pos, matches('Change')
# 			)
# 	}
#
# 	output
# }

format_performance <- function(..., session_names = NULL) {

	elements <- list(...)

	if (is.null(session_names)) session_names <- paste('Session', 1:length(elements))

	lapply(1:length(elements), function(i) {

		elements[[i]] %>% with({
			tibble(
				#Session = session_names[i],
				'Tot. records' = total_records,
				'N. reviewed records (% over total)' = glue("{n_reviewed} ({percent(n_reviewed/total_records)})"),
				'Expected efficiency [PrI]' = percent(efficiency) %>% {glue("{.[2]} [{.[1]}, {.[3]}]")},
				'N. positive matches (% over total)' = glue("{obs_positives} ({percent(obs_positives/total_records)})"),
				'Expected sensitivity [PrI]' = percent(sensitivity) %>% {glue("{.[2]} [{.[1]}, {.[3]}]")},
				'Model R^2' = percent(mod_r2) %>% {glue("{.[2]} [{.[1]}, {.[3]}]")}
		) %>%
				mutate_all(as.character) %>%
				tidyr::pivot_longer(everything(), names_to = 'Indicator', values_to = session_names[i]) %>%
				{
					if (i > 1) .$Indicator <- NULL
					.
				}
		})
	}) %>% bind_cols()

}

plot_predictive_densities <- function(session_name,
																			sessions_folder = options("basren.sessions_folder")[[1]]) {

	library(ggridges)

	records_files <- get_session_files(session_name, sessions_folder)$Annotations
	samples_files <- get_session_files(session_name, sessions_folder)$Samples

	pbmclapply(1:length(records_files), function(i) {
		records <- records_files[[i]] %>%
			import_data() %>%
			mutate(Rev_prediction_new = replace(Rev_prediction_new, !is.na(Rev_prediction_new), '*')) %>%
			transmute(
				Pred_Low, Pred_Up,
				ID,
				Target = coalesce_labels(.)
			)

		neg_lim <- with(records, max(Pred_Up[Target %in% 'n']))
		pos_lim <- with(records, min(Pred_Low[Target %in% 'y']))

		samples <- samples_files[[i]] %>% read_rds()

		unique(records$Target) %>% na.omit %>%
			lapply(function(lab) {
				IDs <- records %>% with(ID[Target %in% lab])
				postsamples <- samples[samples$ID %in% IDs, -1] %>%
					as.matrix %>% as.vector %>% sample(size = 5000)

				data.frame(
					Iteration = i,
					Label = lab,
					Samples = postsamples,
					Neg_lim = neg_lim,
					Pos_lim = pos_lim
				)
			}) %>% bind_rows()
	}) %>% bind_rows() %>%
		mutate(
			Label = factor(Label, c('n', 'y', '*'), c('Negative', 'Positive', 'To review'))
			) %>% {
			df <- mutate(., Iteration = factor(Iteration, sort(unique(Iteration), TRUE)))

			unc_range_df <- select(df, -Samples) %>% distinct()

			group_split(df, Iteration, Label) %>%
				lapply(function(g) {
					dens <- density(arm::logit(g$Samples))

					data.frame(
						Iteration = g$Iteration[1],
						Label = g$Label[1],
						Prob = arm::invlogit(dens$x),
						Dens = dens$y
					)
				}) %>% bind_rows() %>%
				ggplot(aes(y = Iteration)) +
				geom_ridgeline(aes(x = Prob, height = Dens, fill = Label, color = Label), alpha = .5, scale = 1) +
				geom_segment(data = unc_range_df, aes(yend = as.numeric(Iteration) + .1, x = Neg_lim, xend = Neg_lim, color = 'Negative')) +
				geom_segment(data = unc_range_df, aes(yend = as.numeric(Iteration) + .1, x = Pos_lim, xend = Pos_lim, color = 'Positive')) +
				geom_label(data = unc_range_df, aes(y = as.numeric(Iteration) - .1, x = Pos_lim, label = Pos_lim)) +
				geom_label(data = unc_range_df, aes(y = as.numeric(Iteration) - .1, x = Neg_lim, label = Neg_lim)) +
				scale_color_manual(values = c("Negative" = "darkred", "Positive" = "steelblue", "To review" = "violet")) +
				scale_fill_manual(values = c("Negative" = "darkred", "Positive" = "steelblue", "To review" = "violet")) +
				theme_minimal() +
				labs(x = 'Positive match probability', y = 'Iteration')
		}
}

format_var_imp <- function(var_imp, as_data_frame = TRUE) {
	var_imp <- var_imp %>%
		transmute(
			Component = str_extract(Term, '^\\w+(?=__)') %>%
				factor(c('ABSTR', 'TITLE', 'AUTH', 'KEYS', 'MESH'),
							 c('Abstract', 'Title', 'Author', 'Keyword', 'Mesh term')),
			Term = str_replace_all(Term, c('^\\w+__' = '', '\\._\\.' = ' & ', '\\.' = ' | ', '_' = ' ')) %>% str_to_title(),
			'Value (on 10K trees)' = signif(Value * 10000, 3),
			RR = signif(exp(estimate), 3) %>% str_remove('\\.?0+$'),
			Statistic = signif(statistic, 3) %>% str_remove('\\.?0+$'),
		)

	if (!as_data_frame) {
		var_imp <- 	with(var_imp, glue('{Term} ({Component}): {`Value (on 10K trees)`} [{Statistic}]'))
	}

	var_imp
}
