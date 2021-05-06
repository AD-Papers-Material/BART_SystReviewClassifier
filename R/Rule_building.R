select_best_rules <- function(trees, stat.filter = NULL, only.terminal = F,
															only.inclusive.rules = F, target.vec, target.data,
															algorithm = c('linear', 'sequential')) {

	algorithm <- match.arg(algorithm)

	add_cumulative <- function(df, with.score = F) {
		df <- mutate(df,
								 cum.rule = Reduce(x = rule, f = function(x, y) paste(y, x, sep = ' | '),  accumulate = TRUE),
								 cum.pos.i = Reduce(x = pos.i, f = c,  accumulate = TRUE) %>% lapply(unique),
								 cum.pos = sapply(cum.pos.i, length),
								 cum.pos.perc = percent(cum.pos / sum(target.vec %in% 'y')),
								 cum.neg.i = Reduce(x = neg.i, f = c,  accumulate = TRUE) %>% lapply(unique),
								 cum.neg = sapply(cum.neg.i, length),
		)

		if (with.score) {
			df <- df %>%
				mutate(
					cum.score = mclapply(1:n(), function(i) {
						suppressWarnings(arm::bayesglm(cbind(cum.pos[i], cum.neg[i]) ~ 1, binomial) %>%
														 	broom::tidy() %>% pull(statistic))
					}) %>% unlist
				)
		} else df
	}


	if (only.terminal) trees <- filter(trees, terminal)

	if (only.inclusive.rules) trees <- trees %>% filter(str_detect(rule, '1'))

	message('- selecting rules...')

	tictoc::tic()

	if (algorithm == 'linear') {
		res <- trees %>%
			group_by(rule) %>%
			summarise(rule = rule[1], depth = depth[1]) %>% # avg = mean(avg)
			mutate(
				pos.i = lapply(rule, function(cr) {
					which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'y')
				}),
				pos = sapply(pos.i, length),
				neg.i = lapply(rule, function(cr) {
					which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'n')
				}),
				neg = sapply(neg.i, length),
				score = pbmclapply(1:n(), function(i) {
					suppressWarnings(arm::bayesglm(cbind(pos[i], neg[i]) ~ 1, binomial) %>%
													 	broom::tidy() %>% pull(statistic))
				}) %>% unlist
				# score =  pbsapply(rule, function(cr) {
				# 	suppressWarnings(arm::bayesglm(I(target.vec == 'y') ~ eval(str2expression(cr)), binomial, target.data) %>%
				# 		broom::tidy() %>% with(statistic[2]))
				# })
			) %>%
			arrange(desc(score)) %>% # alternative: desc(avg)
			add_cumulative() %>%
			group_by(cum.pos) %>% slice_max(score, n = 1, with_ties = F) %>% ungroup()

	} else {
		res <- data.frame()

		pb <- progressBar(min = 0, max = sum(target.vec %in% 'y'), style = "ETA")

		setTxtProgressBar(pb, 0)

		tictoc::tic()
		temp.trees <- suppressWarnings({
			select(trees, rule, depth) %>%
				distinct() %>%
				mutate(
					pos.i = lapply(rule, function(cr) {
						which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'y')
					}),
					pos = sapply(pos.i, length),
					neg.i = lapply(rule, function(cr) {
						which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'n')
					}),
					neg = sapply(neg.i, length),
					score = mclapply(1:n(), function(i) {
						tryCatch(arm::bayesglm(cbind(pos[i], neg[i]) ~ 1, binomial) %>%
										 	broom::tidy() %>% pull(statistic), error = function(e) browser())
					}) %>% unlist

				) %>% filter(pos > 0)
		})

		target.vec.filtered <- target.vec
		target.data.filtered <- target.data

		while (nrow(temp.trees) > 0 & any(temp.trees$pos > 0)) {
			best <- temp.trees %>% filter(pos > 0) %>% slice_max(score, n = 1)
			res <- bind_rows(res, best)

			setTxtProgressBar(pb, sum(res$pos))

			# print(n_distinct(unlist(res$pos.i)))

			logic.filter <- with(target.data.filtered, !eval(str2expression(best$rule)))
			target.vec.filtered <- target.vec.filtered[logic.filter]
			target.data.filtered <- target.data.filtered[logic.filter, ]
			trees <- trees %>% filter(rule %nin% res$rule)

			temp.trees <- select(trees, rule, depth) %>%
				mutate(
					pos.i = lapply(rule, function(cr) {
						which(with(target.data.filtered, eval(str2expression(cr))) & target.vec.filtered %in% 'y')
					}),
					pos = sapply(pos.i, length),
					neg.i = lapply(rule, function(cr) {
						which(with(target.data.filtered, eval(str2expression(cr))) & target.vec.filtered %in% 'n')
					}),
					neg = sapply(neg.i, length)
				) %>% filter(pos > 0)

			if (nrow(temp.trees) > 0) {
				temp.trees <- temp.trees %>%
					mutate(
						score = mclapply(1:n(), function(i) {
							arm::bayesglm(cbind(pos[i], neg[i]) ~ 1, binomial) %>%
								broom::tidy() %>% pull(statistic)
						}) %>% unlist
					)
			}
		}

		res <- add_cumulative(res)

		cat('\n')
	}

	tictoc::toc()

	message('- cleaning redundant rules...')

	res <- res %>%
		mutate(
			is.subset = sapply(1:n(), function(i) {
				sapply(setdiff(1:n(), i), function(j) {
					# if (all(pos.i[[i]] %in% pos.i[[j]])) j
					all(pos.i[[i]] %in% pos.i[[j]])
				}) %>% any # %>% unlist
			})
		) %>% filter(!is.subset) %>%
		add_cumulative(with.score = T)

	if (!is.null(stat.filter)) filter(res, cum.score > stat.filter) else res
}


extract_rules <- function(Model_output, vimp.threshold = 1.25, n.trees = 800, ...) {

	DTM <- Model_output$Annotated_data %>% mutate(
		Target = case_when(
			!is.na(Rev_prediction) ~ Rev_prediction,
			T ~ Predicted_label
		)
	) %>% select(Target, ID) %>%
		right_join(Model_output$DTM %>% select(-Target), by = 'ID')

	Draws <- Model_output$Prediction_matrix

	message('Generating feature dataset')
	tictoc::tic()

	specific.terms <- Model_output$Variable_importance %>% filter(Score > vimp.threshold) %>% pull(Term) %>%
		str_subset('^MESH', negate = T) %>% str_remove('.+__') %>% unique()

	SpecificDTM <- pbmclapply(specific.terms, function(term) {
		factor((select(DTM, matches(paste0('__', term, '$'))) %>% rowSums(na.rm = T) > 0) + 0)
	}) %>% bind_cols() %>% setNames(paste0('V__', specific.terms)) %>%
		mutate_all(~ replace(.x, is.na(.x), 0))

	print(paste('N. features:', ncol(SpecificDTM)))

	message('Computing trees')

	if (n.trees > ncol(Draws)) warning('Number of trees > than number of MCMC draws. Will use all of them.')

	trees <- pbmclapply(sample(1:ncol(Draws), min(n.trees, ncol(Draws))), function(i) {

		df <- data.frame(
			Pred = Draws[,i],
			SpecificDTM
		)

		rpart(Pred ~ ., data = df, control = rpart.control(...)) %>%
			get_tree_rules(eval.ready = T)
	}) %>% bind_rows()

	message('Extracting rules')

	list(
		linear = select_best_rules(trees, target.vec = DTM$Target, target.data = SpecificDTM, only.terminal = T, only.inclusive.rules = T, algorithm = 'lin'),
		seq = select_best_rules(trees, target.vec = DTM$Target, target.data = SpecificDTM, only.terminal = T, only.inclusive.rules = T, algorithm = 'seq')
	)
}

extract_rules <- function(session_path, rebuild_dtm = F, vimp.threshold = 1.25,
													n.trees = 800, ...) {

	files <- get_session_last_files(session_path)

	Records <- import_data(files$Records)
	DTM <- read_rds(files$DTM)

	DTM <- Model_output$Annotated_data %>% mutate(
		Target = case_when(
			!is.na(Rev_prediction) ~ Rev_prediction,
			T ~ Predicted_label
		)
	) %>% select(Target, ID) %>%
		right_join(Model_output$DTM %>% select(-Target), by = 'ID')

	Draws <- Model_output$Prediction_matrix

	message('Generating feature dataset')

	specific.terms <- Model_output$Variable_importance %>% filter(Score > vimp.threshold) %>% pull(Term) %>%
		str_subset('^MESH', negate = T) %>% str_remove('.+__') %>% unique()

	SpecificDTM <- pbmclapply(specific.terms, function(term) {
		factor((select(DTM, matches(paste0('__', term, '$'))) %>% rowSums(na.rm = T) > 0) + 0)
	}) %>% bind_cols() %>% setNames(paste0('V__', specific.terms)) %>%
		mutate_all(~ replace(.x, is.na(.x), 0))

	print(paste('N. features:', ncol(SpecificDTM)))

	tictoc::toc()

	message('Computing trees')

	if (n.trees > ncol(Draws)) warning('Number of trees > than number of MCMC draws. Will use all of them.')

	trees <- pbmclapply(sample(1:ncol(Draws), min(n.trees, ncol(Draws))), function(i) {

		df <- data.frame(
			Pred = Draws[,i],
			SpecificDTM
		)

		rpart(Pred ~ ., data = df, control = rpart.control(...)) %>%
			get_tree_rules(eval.ready = T)
	}) %>% bind_rows()

	message('Extracting rules')

	list(
		linear = select_best_rules(trees, target.vec = DTM$Target, target.data = SpecificDTM, only.terminal = T, only.inclusive.rules = T, algorithm = 'lin'),
		seq = select_best_rules(trees, target.vec = DTM$Target, target.data = SpecificDTM, only.terminal = T, only.inclusive.rules = T, algorithm = 'seq')
	)
}

rules_to_query <- function(rules) {

	str_remove_all(rules, '^V__') %>% sapply( function(r) {

		rules <- str_split(r, ' & ') %>% unlist %>% sapply(function(x) {
			if (str_detect(x, '.?_.?')) paste0('(', str_replace_all(x, fixed('._.'), ' '), ')') else x
		})

		neg.rules <- str_subset(rules, '%in% "0"')

		rules <- str_subset(rules, '%in% "1"') %>% paste(collapse = ' AND ')

		if (length(neg.rules) > 0) rules <- paste(rules, 'NOT', paste(neg.rules, collapse = ' NOT '))

		rules <- str_remove_all(rules, ' %in% "[01]"') %>% str_squish()
		paste0('(', rules, ')')
	}) %>% {paste0('(', paste(., collapse = ' OR '), ')')}

}
