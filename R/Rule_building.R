extract_rules <- function(session_name, rebuild_dtm = F, vimp.threshold = 1.25,
													n.trees = 800, session_folder = 'Sessions', ...) {

	message('Preparing the data')

	files <- get_session_files(session_name) %>%
		lapply(last) # get only last files

	Records <- import_data(files$Annotations)

	DTM <- if (rebuild_dtm | is.null(files$DTM)) {
		message('Rebuilding the DTM')

		create_training_set(Records)
	} else read_rds(files$DTM)

	Draws <- read_rds(files$Samples)

	Variable_importance <- import_data(files$Annotations, sheet = 'Variable_importance')

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
}

add_cumulative <- function(data, order_by = 'score', rule_var = 'rule') {

	arrange(data, desc(get(order_by)), str_count(get(rule_var), ' & ')) %>%
		mutate(
			cum_pos = purrr::map_int(1:n(), ~ n_distinct(unlist(pos_i[1:.]))),
			cum_neg = purrr::map_int(1:n(), ~ n_distinct(unlist(neg_i[1:.]))),
			.after = any_of(order_by)
		)
}


generate_rule_selection_set <- function(rules, target_vec, target_data, add_negative_terms = TRUE,
																				simplify = TRUE, save_path = NULL) {

	rules <- rules[str_detect(rules, '"1"')] %>%
		tidytrees::simplify_rules() %>% unique()

	if (add_negative_terms) {
		rules <- add_negative_terms(rules, target_vec, target_data) %>%
			select(rule, neg_term = term) %>%
			bind_rows(
				data.frame(rule = rules, neg_term = NA)
			) %>%
			arrange(rule, !is.na(neg_term)) %>%
			group_by(rule) %>%
			mutate(
				rule_with_negs = purrr::map_chr(1:n(), ~ {
					terms <- na.omit(neg_term[1:.])

					if (length(terms) > 0) {
						paste(rule[1], '&', paste(terms, '%in% "0"', collapse = ' & '))
					} else rule[1]
				})
			) %>% pull(rule_with_negs)
	}

	message('- computing scores')

	rules <- rules %>%
		pblapply(function(rule) {

			filt <- with(target_data, eval(str2expression(rule)))

			# if (!is.na(neg_term)) {
			# 	filt <- filt & target_data[[neg_term]] %in% "0"
			# }

			pos_bool = (target_vec %in% 'y' & filt)
			neg_bool = (target_vec %in% 'n' & filt)

			pos = sum(pos_bool)
			neg = sum(neg_bool)

			tibble(
				rule, pos, neg,
				score = pos - neg,
				pos_i = list(which(pos_bool)),
				neg_i = list(which(neg_bool))
			)
		}) %>% bind_rows()

	rules <- rules %>%
		add_cumulative() %>%
		group_by(cum_pos) %>%
		mutate(selected_rule = c(T, rep(F, n() - 1)), .before = rule) %>%
		summarise_all(~ c(.x, NA)) %>%
		mutate(cum_pos = replace(cum_pos, is.na(rule), NA)) %>%
		relocate(cum_pos, .after = score) %>%
		ungroup()

	# if (simplify) {
	# 	message('- simplifying rules')
	# 	rules <- simplify_ruleset(rules) %>%
	# 		generate_rule_selection_set(target_vec = target_vec, target_data = target_data,
	# 																add_negative_terms = FALSE, simplify = FALSE)
	# }

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

		if (length(terms) == 0) {
			return(data.frame(
				rule, term = NA, term_type = NA, pos = NA, neg = NA, neg_i = NA
			))
		}

		terms <- lapply(terms, function(v) {
			pos_bool = (target_vec %in% 'y' & target_data[[v]] %in% '1')
			neg_bool = (target_vec %in% 'n' & target_data[[v]] %in% '1')

			pos = sum(pos_bool)
			neg = sum(neg_bool)

			tibble(
				term = v,
				term_type = term_pos_dict[str_remove(v, fixed('V__'))],
				pos, neg,
				neg_i = list(which(neg_bool))
			)
		}) %>% bind_rows() %>%
			# 	{tryCatch(
			# 	filter(., pos == 0 & neg > 0), error = function(e) browser()
			# )} %>%
			filter(pos == 0 & neg > 0) %>%
			mutate(
				rule = rule,
				#'pos/neg' = glue('{tot_pos}/{tot_neg}'),
				.before = 1
			) %>% mutate(selected_term = FALSE, .after = term)

		if (nrow(terms) == 0) {
			return(data.frame(
				rule, term = NA, term_type = NA, pos = NA, neg = NA, neg_i = NA
			))
		}

		terms <- arrange(terms, desc(neg)) %>%
			mutate(id = 1:n())

		terms$selected_term[1] <- TRUE

		rep <- T

		while (rep) {

			terms[!terms$selected_term,] %>% with({
				neg_i <- sapply(neg_i, setdiff, unlist(terms[terms$selected_term,]$neg_i))
				neg <- sapply(neg_i, length)

				if (length(neg) > 0) {
					id <- id[which(neg > 0 && neg == max(neg))]
				} else id <- integer()

				if (length(id) > 0) {
					terms$selected_term[terms$id == id] <<- TRUE
				} else rep <<- FALSE
			})
		}

		terms[terms$selected_term,]
	}) %>% bind_rows() %>% select(-id, -selected_term)
}


## update the indexes after the simplifications, remove the old rules
simplify_ruleset <- function(ruleset, target_vec, target_data) {

	remove_redundant_rules <- function(data) {
		indexes <- data$pos_i
		subsets <- c()
		tot_pos = indexes %>% unlist() %>% n_distinct()
		for (i in (1:nrow(data))[order(data$score)]) {
			new_tot <- indexes[-i] %>% unlist %>% n_distinct()
			if (new_tot == tot_pos) {
				indexes <- indexes[-i]
				subsets <- c(subsets, i)
			}
		}
		add_cumulative(data[-subsets,])
	}

	ruleset %>%
		ungroup() %>%
		filter(selected_rule %in% TRUE) %>%
		select(-selected_rule) %>%
		mutate(across(rule, str_squish)) %>%
		remove_redundant_rules() %>%
		summarise(
			local({
				tot_pos <- last(cum_pos)
				tot_neg <- last(cum_neg)

				pos_i <- neg_i <- vector("list", n())
				pos <- neg <- score <- vector('integer', n())

				removed_terms = rep(NA, n())
				pb = progressBar()

				tick = 0

				for (i in n():1) {
					pieces <- str_split(rule[i], ' & ') %>% unlist()

					neg_ind <- c()

					for (j in 1:length(pieces)) {
						new_rules <- rule

						new_rules[i] <- paste(pieces[-c(j, neg_ind)], collapse = ' & ')

						new_rules <- new_rules[new_rules != '']

						filt_query <- paste0('(', new_rules, ')', collapse = ' | ')

						res <- target_vec[with(target_data, eval(str2expression(filt_query)))]

						if (sum(res == 'y') >= tot_pos & sum(res == 'n') <= tot_neg) {

							neg_ind <- c(neg_ind, j)

							tot_pos <- sum(res == 'y')
							tot_neg <- sum(res == 'n')
						}
					}

					if (length(neg_ind) > 0) {
						removed_terms[i] <- paste(pieces[neg_ind], collapse = ' & ')
						rule[i] <- paste(pieces[-neg_ind], collapse = ' & ')
					}

					filt <- with(target_data, eval(str2expression(rule[i])))

					pos_bool = (target_vec %in% 'y' & filt)
					neg_bool = (target_vec %in% 'n' & filt)

					pos[i] <- sum(pos_bool)
					neg[i] <- sum(neg_bool)
					pos_i[[i]] <- which(pos_bool)
					neg_i[[i]] <- which(neg_bool)

					tick = tick + 1
					setTxtProgressBar(pb, tick/n())
				}

				tibble(
					rule = rule,
					removed_terms,
					pos, neg, score = pos - neg,
					pos_i, neg_i
				)
			})
		) %>%
		remove_redundant_rules()
}

rules_to_query <- function(rules) {

	str_remove_all(rules, '\\bV__') %>% sapply( function(r) {

		rules <- str_split(r, ' & ') %>% unlist %>% sapply(function(x) {
			if (str_detect(x, '\\.?_\\.?')) {
				x <- str_replace_all(x, '\\.?_\\.?', ' ')
			}

			x <- str_replace(x, '\\.', ' OR ')

			if (str_detect(x, ' (?![%"])')) paste0('(', x, ')') else x
		})

		rule_lenght = length(rules)

		neg.rules <- str_subset(rules, '%in% "0"')

		rules <- str_subset(rules, '%in% "1"') %>% paste(collapse = ' AND ')

		if (length(neg.rules) > 0) {
			rules <- paste(rules, 'NOT', paste(neg.rules, collapse = ' NOT '))
		}

		rules <- str_remove_all(rules, ' %in% "[01]"') %>% str_squish()

		if (rule_lenght > 1) paste0('(', rules, ')') else rules

	}) %>% {paste0('(', paste(., collapse = ' OR '), ')')}

}
