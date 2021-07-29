compute_rnd_sample_needed <- function(obs_pos, obs_neg, tot_records, tot_pos = obs_pos,
																			tot_neg = tot_records - tot_pos,
																			quants = c(.05, .95)) {
	n_needed <- extraDistr::qnhyper(quants, tot_neg, tot_pos, obs_pos)

	list(
		n_needed = n_needed,
		used_prop = (obs_pos + obs_neg) / n_needed,
		needed_prop = n_needed / (obs_pos + obs_neg)
	)
}

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
