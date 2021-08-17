# compute_rnd_sample_needed <- function(obs_pos, obs_neg, tot_records, tot_pos = obs_pos,
# 																			tot_neg = tot_records - tot_pos,
# 																			quants = c(.01, .99)) {
# 	n_needed <- extraDistr::qnhyper(quants, tot_neg, tot_pos, obs_pos)
#
# 	list(
# 		n_needed = n_needed,
# 		used_prop = (obs_pos + obs_neg) / n_needed,
# 		needed_prop = n_needed / (obs_pos + obs_neg),
# 		efficiency = 1 - (obs_pos + obs_neg) / n_needed
# 	)
# }

compute_changes <- function(Annotations) {
	Annotations %>%
		transmute(
			Target = coalesce_labels(cur_data(), c('Rev_prediction_new',
																						 'Rev_prediction', 'Rev_manual')),
			Change = paste(
				coalesce_labels(cur_data(), c('Rev_prediction', 'Rev_manual')),
				Target, sep = ' -> ') %>% str_replace_all('NA', 'unlab.')
		) %>% {
			df <- .
			lapply(names(df), function(col) {

				df %>%
					transmute(Col = get(col) %>% factor()) %>%
					filter(!is.na(Col)) %>%
					count(Col) %>%
					tidyr::pivot_wider(names_from = Col, values_from = n,
														 names_prefix = paste0(col, ': '))
			}) %>% bind_cols() %>%
				mutate(
					Total_labeled = sum(!is.na(df$Target)),
					New_labels = if ('Rev_prediction_new' %in% names(Annotations)) {
						sum(!is.na(Annotations$Rev_prediction_new))
					} else NA,
					across(c(Total_labeled, New_labels), ~ if (!is.na(.x)) {
						x <- .x # Some changes in glue or dplyr made glue not recognizing .x anymore
						glue('{x} ({percent(x/nrow(Annotations))})') %>% as.character()
					} else NA),
					.after = matches('Target')
				)
		}
}

# estimate_positivity_rate_model <- function(train_data) {
# 	library(brms)
#
# 	train_data <- train_data %>%
# 		transmute(
# 			Target = coalesce_labels(., c('Rev_manual', 'Rev_prediction',
# 																		'Rev_prediction_new', 'Rev_previous')) %>% factor(),
# 			Order
# 			) %>%
# 		na.omit()
#
# 	brm(bf(Target ~ a * Order ^ b, a + b ~ 1, nl = T), family = bernoulli,
# 						data = train_data,
# 						cores = 8, chains = 8, refresh = 0, iter = 8000, control = list(adapt_delta = .95),
# 						backend = 'cmdstan',
# 						prior = c(set_prior('student_t(1, 0, 5)', nlpar = "a"),
# 											set_prior('student_t(1, 0, 2.5)', nlpar = "b")))
#
# }

estimate_positivity_rate_model <- function(train_data, seed = 14129189) {
	library(brms)

	train_data$Target = factor(coalesce_labels(train_data))

	brm(bf(Target ~ Pred_Low), family = bernoulli,
			data = train_data,
			cores = 8, chains = 8, refresh = 0, iter = 8000, control = list(adapt_delta = .95),
			backend = 'cmdstan', seed = seed,
			prior = c(
				prior(student_t(3, 0, 2.5), class = "Intercept"),
				prior(student_t(3, 0, 1.5), class = "b")
			))

}

# predict_num_positives <- function(model, tot_records, start_index = 1,
# 																	nsamples = min(2000, sum(model$fit@sim$n_save))) {
# 	posterior_predict(mod, newdata = data.frame(Order = start_index:tot_records), nsamples = nsamples) %>%
# 		rowSums()
# }

estimate_performance <- function(records, model = NULL, preds = NULL, plot = TRUE,
																 quants = options('baysren.probs')[[1]],
																 nsamples = min(2500, sum(model$fit@sim$n_save)),
																 seed = 23797297,
																 save_preds = FALSE, save_model = FALSE) {

	if (is.null(model)) {
		message('- build model...')
		model <- estimate_positivity_rate_model(records, seed)
	}

	quants <- sort(quants)

	records <- arrange(records, Order)

	tot_records = nrow(records)

	if (is.null(preds)) {
		message('- computing predictions...')
		set.seed(seed)
		preds <- posterior_predict(model, newdata = records, nsamples = nsamples)
	}

	message('- integrate with observed data...')
	vec <- as.numeric(records$Target == 'y')
	#pb <- progressBar()

	for (i in which(!is.na(records$Target))) {
		preds[,i] <- vec[i]
		#setTxtProgressBar(pb, i / length(vec))
	}
	#close(pb)

	n_sims <- nrow(preds)
	obs_pos <- sum(records$Target %in% 'y')
	tot_reviewed <- sum(!is.na(records$Target))
	tot_pos <- pmax(preds %>% rowSums(), obs_pos)
	tot_neg <- tot_records - tot_pos

	n_needed <- extraDistr::rnhyper(n_sims, tot_neg, tot_pos, obs_pos)

	res <- list(
		obs_positives = obs_pos,
		pred_positives = quantile(tot_pos, quants),
		mod_r2 = bayes_R2(model, probs = quants)[3:5],
		n_reviewed = tot_reviewed,
		total_records = tot_records,
		used_prop = quantile(tot_reviewed / n_needed, quants),
		efficiency = quantile(1 - tot_reviewed / n_needed, quants),
		sensitivity = quantile(obs_pos / tot_pos, quants)
	) %>%
		sapply(function(el) { # if quantile, sort them
			if (length(el) == 3) {
				sort(el) %>% setNames(percent(quants))
			} else el
		})

	if (save_preds) {
		res$preds <- preds
	}

	if (save_model) {
		res$model <- model
	}

	if (plot) {
		cl <- suppressWarnings(makeCluster(getOption("mc.cores")))
		on.exit(stopCluster(cl))
		message('- computing cumulative trends...')

		preds <- preds %>% parApply(1, cumsum, cl = cl)

		message('- extracting cumulative quantiles...')

		preds <- parApply(preds, 1, quantile, quants, cl = cl) %>% t %>%
			as.data.frame() %>%
			setNames(c('L', 'M', 'U'))

		res$plot <- preds %>%
			mutate(
				Target = cumsum(records$Target %in% 'y'),
				across(c(Target, L, M, U), ~ tapply(.x, .x, function(x) c(x[1], rep(NA, length(x) - 1))) %>% unlist()),
				max = coalesce(Target, L, M, U),
				Order = 1:n()
			) %>% filter(!is.na(max)) %>%
			mutate(
				across(c(L, M, U), ~ sapply(1:length(.x), function(i) {
					if (is.na(.x[i])) max(.x[1:i], na.rm = T) else .x[i]
				}))
			) %>% ggplot(aes(Order)) +
			geom_ribbon(aes(ymin = L, ymax = U, color = glue('{diff(range(quants)) * 100}% Predictive Interval')), alpha = .1) +
			geom_line(aes(y = M, color = 'Predictive Median')) +
			geom_point(aes(y = Target), color = 'darkred') +
			theme_minimal() +
			#scale_x_continuous(trans = 'log', labels = round, breaks = function(x) seq(0, log(tot_records), length.out = 10) %>% exp()) +
			scale_color_manual(values = c('steelblue', 'blue')) +
			guides(alpha = 'none') +
			labs(x = 'Records', y = 'Cum. positive matches', color = '')

		#print(res$plot)
	}

	res
}

# plot_predicted_pos_rate <- function(Ann_data, block_size = 50) {
#
# 	# plot_predicted_pos_rate(a %>% mutate(Rev_previous = coalesce_labels(., c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous')) %>% replace(Order > 1200, NA)), block_size = 50)
#
# 	Ann_data <- Ann_data %>%
# 		transmute(
# 			Order,
# 			Train = Rev_previous == 'y',
# 			Target = coalesce_labels(., c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous')) == 'y'
# 		) %>%
# 		group_by(Block = ceiling(1:n()/block_size)) %>%
# 		summarise(
# 			Order = last(Order),
# 			block_size = n(),
# 			Train = sum(Train),
# 			Target = sum(Target, na.rm = T)
# 		)
# 	#
# 	# mod <- brm(Train | trials(block_size) ~ Order, family = binomial, data = Ann_data,
# 	# 					 cores = 8, refresh = 0, iter = 8000,
# 	# 					 backend = 'cmdstan',
# 	# 					 prior = c(prior(student_t(1, 0, 5), class = 'Intercept'),
# 	# 					 					prior(student_t(1, 0, 2.5), class = 'b')))
#
# 	mod <- brms::brm(bf(Train | trials(block_size) ~ a * Order ^ b, a + b ~ 1, nl = T), family = binomial, data = Ann_data,
# 									 cores = 8, refresh = 0, iter = 8000,
# 									 backend = 'cmdstan',
# 									 prior = c(set_prior('student_t(1, 0, 5)', nlpar = "a"),
# 									 					set_prior('student_t(1, 0, 2.5)', nlpar = "b")))
#
# 	## To estimate the total number of positives and from there the positives in a
# 	## random subsample
# 	# posterior_predict(mod, newdata = data.frame( block_size,
# 	# Order = seq(1200, nrow(a), block_size) )) %>% rowSums() %>% quantile(c(.01,
# 	# .05, .5, .95, .99)) %>% {qhyper(p = .95, (96+ .)/nrow(a) * 1200, 1200 * (1 -
# 	# (96+ .)/nrow(a)), 500)} print(mod)
# browser()
# 	posterior_predict(mod, newdata = data.frame(
# 		block_size,
# 		Order = seq(block_size, limit, block_size)
# 	)) %>% rowSums() %>% quantile(seq(0,1, .1)) %>%
# 		print()
#
# 	limit <- min(Ann_data$Order[last(which(Ann_data$Target > 0))], last(Ann_data$Order))
#
# 	train_limit = max(Ann_data$Order[!is.na(Ann_data$Train)])
#
# 	data.frame(
# 		block_size,
# 		Order = seq(block_size, limit, block_size)
# 	) %>%
# 		left_join(Ann_data) %>%
# 		mutate(
# 			Target_lab = Target,
# 			Target = Target / block_size,
# 			(posterior_predict(mod, newdata = cur_data()) / block_size) %>%
# 				apply(2, quantile, c(.05, .5, .95)) %>% t %>%
# 				as.data.frame() %>%
# 				setNames(c('Low', 'Med', 'Up')),
# 			across(c(Target_lab, Target), ~ replace(.x, .x == 0, NA))
# 		) %>%
# 		ggplot(aes(Order)) +
# 		geom_ribbon(aes(ymin = Low, ymax = Up), alpha = .25) +
# 		geom_vline(xintercept = train_limit, linetype = 'dashed', alpha = .5) +
# 		geom_segment(aes(xend = Order, y = 0, yend = Target, color = 'Obs.'), size = 1) +
# 		geom_line(aes(y = Med, color = 'Pred.'), size = 1, linetype = 'dashed', alpha = .8) +
# 		geom_label(aes(y = Target, label = Target_lab)) +
# 		theme_minimal() +
# 		scale_x_continuous(breaks = seq(block_size, limit, block_size)) +
# 		scale_y_continuous(trans = 'log1p', breaks = seq(0, .3, .01)) +
# 		labs(y = 'Pos. rate', x = 'Records', color = NULL) +
# 		theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
# 	# ggplot(Ann_data, aes(Order)) +
# 	# 	#geom_line(aes(y = Pred_Up, color = 'Up')) +
# 	# 	#geom_line(aes(y = Pred_Low, color = 'Low')) +
# 	# 	geom_errorbar(aes(ymin = Pred_Low, ymax = Pred_Up, color = coalesce_labels(a)), width = 0) +
# 	# 	geom_point(aes(y = Pred_Med, color = Predicted_label), alpha = .8) +
# 	# 	scale_y_continuous(trans = 'logit') + theme_minimal()
# }

extract_var_imp <- function(session_name, num_vars = 15, score_filter = 1.5, recompute_DTM = FALSE,
														sessions_folder = options("baysren.sessions_folder")[[1]]) {

	message('Retrieving data')
	session_files <- get_session_files(session_name, sessions_folder)

	Records <- last(session_files$Annotations) %>% import_data() %>%
		mutate(Target = coalesce_labels(.) %>% tidyr::replace_na('n')) %>%
		select(ID, Target)

	Variables <- last(session_files$Annotations) %>%
		import_data(sheet = 'Variable_importance') %>%
		filter(!str_detect(Term, '^\\w+\\.count'), Score > score_filter) %>%
		arrange(desc(Value)) %>%
		head(num_vars)

	if (recompute_DTM) {
		DTM <- create_training_set(Records)
	} else DTM <- read_rds(session_files$DTM)

	DTM$Target <- as.numeric(Records[match(DTM$ID, Records$ID),]$Target == 'y')

	message('Computing variable importance')
	pbmclapply(1:nrow(Variables), function(i) {
		Term_data <- Variables[i,]
		term <- Term_data$Term

		data.frame(
			Term_data,
			glm(Target ~ get(term), poisson(), DTM) %>% broom::tidy() %>%
				tail(-1) %>% select(-term, -std.error, -p.value)
		)
	}) %>% bind_rows()

}

analyse_grid_search <- function(session_folder = 'Grid_Search', tot_pos = NULL,
																tot_records = NULL,  plot = TRUE,
																score = c('Sens_adj_eff', 'Pos_rate_adj_sens',
																					'Pos_rate')) {

	score <- match.arg(score)

	if (is.null(tot_pos) | is.null(tot_records)) {
		Labels <- list.files(session_folder, pattern = 'Records_', recursive = T, full.names = T)[1] %>%
			import_data() %>%
			mutate(
				Target = coalesce_labels(., label_cols = c('Rev_prediction_new',
																									 'Rev_prediction', 'Rev_manual',
																									 'Rev_previous'))
			) %>% with(table(Target)) %>% as.list()


		if (is.null(tot_pos)) tot_pos <- Labels$y
		if (is.null(tot_records)) tot_records <- sum(unlist(Labels))
	}

	out <- list.files(session_folder, pattern = 'Results_', recursive = T, full.names = T) %>%
		pbmclapply(function(file) {

			readr::read_csv(file, col_types = cols()) %>%
				tidyr::pivot_wider(names_from = Indicator, values_from = Value) %>%
				transmute(
					Iter,
					Session = file,
					Rep = `Replication n.`,
					Tot_labeled = Total_labeled,
					Pos_labels = `Target: y`
				)
		}) %>% bind_rows() %>%
		mutate(
			Session = str_remove(Session, 'Results.*') %>% basename(),
			Tot_labeled = str_remove(Tot_labeled, ' \\(.*'),
			Session %>% str_remove('.*GridSession\\.') %>%
				str_split('\\.', simplify = T) %>%
				as.data.frame() %>%
				lapply(function(piece) {

					label <- str_remove(piece, '^(\\d+|[yn])')[1]
					value <- str_remove(piece, fixed(label))

					setNames(data.frame(value), label)
				}) %>% bind_cols(),
			across(one_of(c("Tot_labeled", "Pos_labels", "Mods", "Quant", 'Init',
											'Mult')), as.numeric),
			Pos_rate = Pos_labels / Tot_labeled,
			Sensitivity = Pos_labels / tot_pos,
			Efficiency = 1 - Tot_labeled / tot_records,
			Pos_rate_adj_sens = Pos_rate * Sensitivity,
			Sens_adj_eff = Sensitivity * Efficiency,
			Score = get(score)
		) %>%
		group_by(Session) %>%
		slice_tail(n = 1) %>%
		ungroup()

	params <- c("Mods", "Quant", "Resamp", "Init", "Mult")

	tree <- out %>%
		select(Score, one_of(c("Mods", "Quant", "Resamp", "Init", "Mult"))) %>%
		mutate_at(vars(-Score), as.factor) %>% {
			df <- .
			rpart(Score ~ ., df)
		}

	rules <- tidytrees::tidy_tree(tree)[tree$where - 1,] %>%
		mutate(
			rule = sprintf('%s. %s (%.2g)',
										 as.numeric(factor(rule, unique(rule[order(estimate, decreasing = T)]))),
										 rule, estimate),
			rule = factor(rule, unique(rule[order(estimate, decreasing = T)]))
		)

	out <- mutate(out, Rule = rules$rule)

	p <- NULL

	if (plot) {

		if (R.version$major >= 4) {
			f <- palette()[1:7] %>% colorRampPalette()
			colors <- f(n_distinct(out$Rule)) %>% rev()
		} else colors <- viridis::cividis(n_distinct(out$Rule))

		p <- lapply(params, function(par) {
			params <- setdiff(params, par)

			out %>%
				mutate(group = select(cur_data(), one_of(params)) %>%
							 	apply(1, paste, collapse = ' ')) %>%
				ggplot(aes(factor(get(par)), Score)) +
				geom_boxplot(show.legend = F, outlier.shape = NA) +
				geom_line(aes(group = group, color = Rule), alpha = .5, show.legend = F) +
				geom_boxplot(aes(fill = Rule), show.legend = F, outlier.shape = NA) +
				#geom_line(aes(group = group, color = Rule), alpha = .5, show.legend = F) +
				geom_point(aes(color = Rule), show.legend = F) +
				theme_minimal() +
				scale_y_continuous(labels = function(x) round(x, 2)) +
				scale_color_manual(values = colors) +
				scale_fill_manual(values = colors) +
				ylab(score) +
				xlab(par)
		})

		p_one <- out %>%
			ggplot(aes(factor(get(params[1])), Score)) +
			geom_line(aes( group = Rule, color = Rule), size = 2) +
			theme_minimal() +
			scale_color_manual(values = colors) +
			scale_fill_manual(values = colors) +
			labs(color = glue('Perf. cluster (mean {score})'))

		tmp <- ggplot_gtable(ggplot_build(p_one))
		leg <- which(tmp$layout$name == 'guide-box')

		legend <- tmp$grobs[[leg]]

		p <- patchwork::wrap_plots(p) + legend

		#print(p)
	}

	list(
		iterations = out,
		best_parms = out %>% filter(str_detect(Rule, '^1\\.')) %>%
			arrange(desc(Sensitivity), desc(Efficiency)) %>% head(1) %>%
			# slice_max(Sensitivity, n = 1, with_ties = T) %>%
			# slice_max(Score, n = 1, with_ties = F) %>%
			select(Iter, Rep, Pos_labels, Sensitivity, Tot_labeled, Efficiency,
						 Score, any_of(params)) %>%
			mutate(
				Score = glue("{signif(Score, 3)} ({score})"),
				Pos_labels = glue("{Pos_labels} / {tot_pos}"),
				Sensitivity = percent(Sensitivity),
				Tot_labeled = glue("{Tot_labeled} / {tot_records}"),
				Efficiency = percent(Efficiency),
				across(.fns = as.character)) %>%
			tidyr::pivot_longer(everything(), names_to = 'Parameter', 'Value'),
		best_by_rule = out %>% group_by(Cluster = Rule) %>%
			slice_max(Score, n = 1, with_ties = T) %>%
			slice_max(Sensitivity, n = 1, with_ties = T) %>%
			slice_max(Score, n = 1, with_ties = F) %>%
			# ungroup() %>%
			# arrange(desc(Score), desc(Sensitivity), desc(Efficiency)) %>%
			select(Cluster, Iter, Pos_labels, Tot_labeled, Sensitivity, Efficiency,
						 Score, any_of(params)) %>%
			mutate(
				Pos_labels = glue("{Pos_labels} / {tot_pos}"),
				Tot_labeled = glue("{Tot_labeled} / {tot_records}"),
				Sensitivity = percent(Sensitivity),
				Efficiency = percent(Efficiency),
				Score = glue("{signif(Score, 3)} ({score})"),
				across(.fns = as.character)),
		plot = p
	)
}
