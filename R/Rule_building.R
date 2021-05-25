generate_rule_selection_set <- function(rules, target_vec, target_data, add_negative_terms = TRUE,
															save_path = NULL) {

	rules <- rules[str_detect(rules, '"1"')] %>%
		tidytrees::simplify_rules() %>% unique()

	if (add_negative_terms) {
		rules <- add_negative_terms(rules, target_vec, target_data) %>%
			select(rule, neg_term = term) %>%
			bind_rows(
				data.frame(rule = rules, neg_term = NA)
			) %>%
			arrange(rule, is.na(neg_term))
	} else {
		rules <- data.frame(rule = rules, neg_term = NA)
	}

	message('- ordering rules')

	rules <- rules %>%
		rowwise() %>%
		summarise({

			filt <- with(target_data, eval(str2expression(rule)))

			if (!is.na(neg_term)) {
				filt <- filt & target_data[[neg_term]] %in% "0"
			}

			pos_bool = (target_vec %in% 'y' & filt)
			neg_bool = (target_vec %in% 'n' & filt)

			pos = sum(pos_bool)
			neg = sum(neg_bool)

			tibble(
				rule, neg_term, pos, neg,
				score = pos - neg,
				pos_i = list(which(pos_bool)),
				neg_i = list(which(neg_bool))
			)
		}, .groups = 'drop') %>%
		#mutate('pos/neg' = glue('{pos}/{neg}'), .after = rule) %>%
		arrange(desc(score), str_count(rule, ' & ')) %>%
		mutate(
			cum_pos = purrr::map_int(1:n(), ~ n_distinct(unlist(pos_i[1:.]))),
			cum_neg = purrr::map_int(1:n(), ~ n_distinct(unlist(neg_i[1:.]))),
			.after = score
		) %>%
		group_by(cum_pos) %>%
		mutate(selected_rule = c(T, rep(F, n() - 1)), .after = neg_term) %>%
		ungroup()

	if (!is.null(save_path)) {
		openxlsx::write.xlsx(rules, save_path)
	}

	rules
}

add_negative_terms <- function(rules, target_vec, target_data) {
	message('- retrieving negative terms')
	pbmclapply(rules, function(rule) {

		filt <- with(target_data, which(eval(str2expression(rule))))

		target_data <- target_data[filt,] %>% select(where(~ n_distinct(.x) > 1))

		target_vec <- target_vec[filt]
		tot_pos <- sum(target_vec == 'y')
		tot_neg <- sum(target_vec == 'n')

		terms <- colnames(target_data) %>%
			str_subset('V__\\d+$', negate = T) %>%
			str_subset('V__\\w{1,3}$', negate = T)

		term_pos_dict <- lexicon::hash_grady_pos %>% with(setNames(pos, word))
		excluded_pos <- term_pos_dict[term_pos_dict %nin% c('Adjective', 'Noun', 'Plural', 'Noun Phrase')]

		terms <- terms[str_remove(terms, 'V__') %nin% names(excluded_pos)]

		terms <- lapply(terms, function(v) {
			pos_bool = (target_vec %in% 'y' & target_data[[v]] %in% '1')
			neg_bool = (target_vec %in% 'n' & target_data[[v]] %in% '1')

			pos = sum(pos_bool)
			neg = sum(neg_bool)

			tibble(
				term = v,
				term_type = term_pos_dict[str_remove(v, fixed('V__'))],
				pos, neg,
				delta = neg - pos,
				#pos_i = list(which(pos_bool)),
				neg_i = list(which(neg_bool))
			)
		}) %>% bind_rows() %>%
			filter(pos == 0 & neg > 0) %>%
			mutate(
				rule = rule,
				'pos/neg' = glue('{tot_pos}/{tot_neg}'),
				.before = 1
			) %>% mutate(selected_term = FALSE, .after = term)

		if (nrow(terms) > 0) {
			terms <- arrange(terms, desc(neg)) %>%
				mutate(id = 1:n())

			terms$selected_term[1] <- TRUE

			rep <- T

			while (rep) {

				terms[!terms$selected_term,] %>% with({
					neg_i <- sapply(neg_i, setdiff, unlist(terms[terms$selected_term,]$neg_i))
					neg <- sapply(neg_i, length)

					id <- id[which(neg > 0 && neg == max(neg))]

					if (length(id) > 0) {
						terms$selected_term[terms$id == id] <<- TRUE
					} else rep <<- FALSE
				})
			}

			terms[terms$selected_term,]
		}
	}) %>% bind_rows() %>% select(-id, -selected_term)
}


# select_best_rules_old <- function(trees, stat.filter = NULL, only.terminal = F,
# 															only.inclusive.rules = F, target.vec, target.data,
# 															algorithm = c('linear', 'sequential')) {
#
# 	algorithm <- match.arg(algorithm)
#
# 	add_cumulative <- function(df, with.score = F) {
# 		df <- mutate(df,
# 								 cum.rule = Reduce(x = rule, f = function(x, y) paste(y, x, sep = ' | '),  accumulate = TRUE),
# 								 cum.pos.i = Reduce(x = pos.i, f = c,  accumulate = TRUE) %>% lapply(unique),
# 								 cum.pos = sapply(cum.pos.i, length),
# 								 cum.pos.perc = percent(cum.pos / sum(target.vec %in% 'y')),
# 								 cum.neg.i = Reduce(x = neg.i, f = c,  accumulate = TRUE) %>% lapply(unique),
# 								 cum.neg = sapply(cum.neg.i, length),
# 		)
#
# 		if (with.score) {
# 			df <- df %>%
# 				mutate(
# 					cum.score = mclapply(1:n(), function(i) {
# 						suppressWarnings(arm::bayesglm(cbind(cum.pos[i], cum.neg[i]) ~ 1, binomial) %>%
# 														 	broom::tidy() %>% pull(statistic))
# 					}) %>% unlist
# 				)
# 		} else df
# 	}
#
# 	if (only.terminal) trees <- filter(trees, terminal)
#
# 	if (only.inclusive.rules) trees <- trees %>% filter(str_detect(rule, '1'))
#
# 	message('- selecting rules...')
#
# 	tictoc::tic()
#
# 	if (algorithm == 'linear') {
# 		res <- trees %>%
# 			group_by(rule) %>%
# 			summarise(rule = rule[1], depth = depth[1]) %>% # avg = mean(avg)
# 			mutate(
# 				pos.i = lapply(rule, function(cr) {
# 					which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'y')
# 				}),
# 				pos = sapply(pos.i, length),
# 				neg.i = lapply(rule, function(cr) {
# 					which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'n')
# 				}),
# 				neg = sapply(neg.i, length),
# 				score = pos - neg,
# 				# score = pbmclapply(1:n(), function(i) {
# 				# 	suppressWarnings(arm::bayesglm(cbind(pos[i], neg[i]) ~ 1, binomial) %>%
# 				# 									 	broom::tidy() %>% pull(statistic))
# 				#
# 				# }) %>% unlist
# 				# score =  pbsapply(rule, function(cr) {
# 				# 	suppressWarnings(arm::bayesglm(I(target.vec == 'y') ~ eval(str2expression(cr)), binomial, target.data) %>%
# 				# 		broom::tidy() %>% with(statistic[2]))
# 				# })
# 			) %>%
# 			arrange(desc(score)) %>% # alternative: desc(avg)
# 			add_cumulative() %>%
# 			group_by(cum.pos) %>% slice_max(score, n = 1, with_ties = F) %>% ungroup()
#
# 	} else {
# 		res <- data.frame()
#
# 		pb <- progressBar(min = 0, max = sum(target.vec %in% 'y'), style = "ETA")
#
# 		setTxtProgressBar(pb, 0)
#
# 		tictoc::tic()
# 		temp.trees <- suppressWarnings({
# 			select(trees, rule, depth) %>%
# 				distinct() %>%
# 				mutate(
# 					pos.i = lapply(rule, function(cr) {
# 						which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'y')
# 					}),
# 					pos = sapply(pos.i, length),
# 					neg.i = lapply(rule, function(cr) {
# 						which(with(target.data, eval(str2expression(cr))) & target.vec %in% 'n')
# 					}),
# 					neg = sapply(neg.i, length),
# 					score = mclapply(1:n(), function(i) {
# 						tryCatch(arm::bayesglm(cbind(pos[i], neg[i]) ~ 1, binomial) %>%
# 										 	broom::tidy() %>% pull(statistic), error = function(e) browser())
# 					}) %>% unlist
#
# 				) %>% filter(pos > 0)
# 		})
#
# 		target.vec.filtered <- target.vec
# 		target.data.filtered <- target.data
#
# 		while (nrow(temp.trees) > 0 & any(temp.trees$pos > 0)) {
# 			best <- temp.trees %>% filter(pos > 0) %>% slice_max(score, n = 1, with_ties = T)
# 			res <- bind_rows(res, best)
#
# 			setTxtProgressBar(pb, sum(res$pos))
#
# 			# print(n_distinct(unlist(res$pos.i)))
#
# 			logic.filter <- with(target.data.filtered, !eval(str2expression(best$rule)))
# 			target.vec.filtered <- target.vec.filtered[logic.filter]
# 			target.data.filtered <- target.data.filtered[logic.filter, ]
# 			trees <- trees %>% filter(rule %nin% res$rule)
#
# 			temp.trees <- select(trees, rule, depth) %>%
# 				mutate(
# 					pos.i = lapply(rule, function(cr) {
# 						which(with(target.data.filtered, eval(str2expression(cr))) & target.vec.filtered %in% 'y')
# 					}),
# 					pos = sapply(pos.i, length),
# 					neg.i = lapply(rule, function(cr) {
# 						which(with(target.data.filtered, eval(str2expression(cr))) & target.vec.filtered %in% 'n')
# 					}),
# 					neg = sapply(neg.i, length)
# 				) %>% filter(pos > 0)
#
# 			if (nrow(temp.trees) > 0) {
# 				temp.trees <- temp.trees %>%
# 					mutate(
# 						score = mclapply(1:n(), function(i) {
# 							arm::bayesglm(cbind(pos[i], neg[i]) ~ 1, binomial) %>%
# 								broom::tidy() %>% pull(statistic)
# 						}) %>% unlist
# 					)
# 			}
# 		}
#
# 		res <- add_cumulative(res)
#
# 		cat('\n')
# 	}
#
# 	tictoc::toc()
#
# 	message('- cleaning redundant rules...')
#
# 	res <- res %>%
# 		mutate(
# 			is.subset = sapply(1:n(), function(i) {
# 				sapply(setdiff(1:n(), i), function(j) {
# 					# if (all(pos.i[[i]] %in% pos.i[[j]])) j
# 					all(pos.i[[i]] %in% pos.i[[j]])
# 				}) %>% any # %>% unlist
# 			})
# 		) %>% filter(!is.subset) %>%
# 		add_cumulative(with.score = T)
#
# 	if (!is.null(stat.filter)) filter(res, cum.score > stat.filter) else res
# }

extract_rules <- function(session_path, rebuild_dtm = F, vimp.threshold = 1.25,
													n.trees = 800, ...) {

	message('Preparing the data')

	files <- get_session_last_files(session_path)

	Records <- import_data(files$Records)

	DTM <- if (rebuild_dtm | is.null(files$DTM)) {
		message('Rebuilding the DTM')

		create_training_set(Records)
	} else read_rds(files$DTM)

	Draws <- read_rds(files$Samples)

	Variable_importance <- import_data(files$Records, sheet = 'Variable_importance')

	DTM <- Records %>% transmute(
		ID,
		Target = coalesce_labels(cur_data(), c('Rev_prediction_new','Rev_prediction',
																					 'Rev_manual', 'Predicted_label'))
	) %>%
		right_join(DTM %>% select(-Target), by = 'ID')

	message('Generating feature dataset')

	specific.terms <- Variable_importance %>% filter(Score > vimp.threshold) %>% pull(Term) %>%
		str_subset('^MESH', negate = T) %>% str_remove('.+__') %>%
		str_sub(1, 2500) %>% # str_sub() is necessary since many functions cannot use such long names
		unique()


	SpecificDTM <- pbmclapply(specific.terms, function(term) {
		values <- select(DTM, matches(paste0('__', term, '$'))) %>% rowSums(na.rm = T)

		factor((values > 0) + 0)
	}) %>%
		setNames(paste0('V__', specific.terms)) %>%
		bind_cols() %>%
		mutate_all(~ replace(.x, is.na(.x), 0)) %>%
		select(where(~ n_distinct(.x) > 1))

	print(paste('N. features:', ncol(SpecificDTM)))

	tictoc::toc()

	message('Computing trees')

	if (n.trees > ncol(Draws)) warning('Number of trees > than number of MCMC draws. All draws will be used')

	# Add the median estimate and ensure it is included among the candidate draws
	Draws <- cbind(Draws[,-1], Records$Pred_Med)
	candidate_draws <- sample(1:ncol(Draws), min(n.trees, ncol(Draws))) %>%
		c(ncol(Draws)) %>% unique()

	trees <- pbmclapply(candidate_draws, function(i) {

		df <- data.frame(
			Pred = Draws[,i],
			SpecificDTM
		)

		rpart(Pred ~ ., data = df, control = rpart.control(...)) %>%
			tidytrees::tidy_tree(eval_ready = T, simplify_rules = T)
	}) %>% bind_rows()

	list(
		SpecificDTM = SpecificDTM,
		DTM = DTM,
		rules = trees$rule
	)
	# message('Extracting rules')
	#
	# list(
	# 	linear = select_best_rules(trees, target.vec = DTM$Target, target.data = SpecificDTM, only.terminal = T, only.inclusive.rules = T, algorithm = 'lin'),
	# 	seq = select_best_rules(trees, target.vec = DTM$Target, target.data = SpecificDTM, only.terminal = T, only.inclusive.rules = T, algorithm = 'seq')
	# )
}

ruleset_to_query <- function(ruleset) {

	str_remove_all(rules, '\\bV__') %>% sapply( function(r) {

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
