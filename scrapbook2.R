mod_batches <- brms::brm(bf(Target | trials(Size) ~ a * Order ^ b, a + b ~ 1, nl = T), family = binomial,
												 data = train_data %>%
												 	mutate(Target = coalesce_labels(., c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous'))) %>%
												 	group_by(Batches = Hmisc::cut2(Order, m = 50)) %>%
												 	summarise(
												 		Order = min(Order),
												 		Target = sum(Target == 'y'),
												 		Size = n()
												 	),
												 cores = 8, chains = 8, refresh = 0, iter = 8000,
												 backend = 'cmdstan',
												 prior = c(set_prior('student_t(1, 1, 5)', nlpar = "a"),
												 					set_prior('student_t(1, 0, 2.5)', nlpar = "b")))

mod <- brms::brm(bf(Target ~ a * Order ^ b, a + b ~ 1, nl = T), family = bernoulli, data = train_data %>%
								 	mutate(Target = coalesce_labels(., c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new', 'Rev_previous')) %>% factor) ,
								 cores = 8, chains = 8, refresh = 0, iter = 8000, control = list(adapt_delta = .95),
								 backend = 'cmdstan',
								 prior = c(set_prior('student_t(1, 0, 5)', nlpar = "a"),
								 					set_prior('student_t(1, 0, 2.5)', nlpar = "b")))

res <- posterior_predict(mod, newdata = data.frame(Order = 1:17755), nsamples = 2000)


local({
	res <- res_batches
	tot_pos <- pmax(res %>% rowSums(), 101)
	tot_neg <- 17755 - tot_pos
	n_needed <- extraDistr::rnhyper(nrow(res), tot_neg, tot_pos, 101)

	print(quantile(tot_pos, c(.05, .5, .95)))
	print(sapply(c(.8, .9, .95), function(q) mean((101/ tot_pos) > q)))
	print(quantile(101 / tot_pos, c(.05, .5, .95)))

	print(quantile(n_needed, c(.05, .5, .95)))

	1 - quantile((766) / n_needed, c(.05, .5, .95))
})


######################
certain_preds <- with(records, sum(Target[Order <= 1200] %in% 'y'))

tot_records = nrow(records)

certain_zone <- res[,1:1200] %>% apply(1, cumsum) %>%
	apply(1, quantile, c(.05, .5, .95)) %>% t %>% as.data.frame() %>%
	setNames(c('L', 'M', 'U'))
uncertain_zone <- (res[,1201:ncol(res)] %>% apply(1, cumsum) + certain_preds) %>%
	apply(1, quantile, c(.05, .5, .95)) %>% t %>% as.data.frame() %>%
	setNames(c('L', 'M', 'U'))


bind_rows(
	certain_zone,
	uncertain_zone
) %>%
	mutate(
		Target = coalesce_labels(records, c('Rev_manual', 'Rev_prediction',
																				'Rev_prediction_new', 'Rev_previous')),
		Target = cumsum(Target %in% 'y'),
		NewPos = tapply(Target, Target, function(x) c(x[1], rep(NA, length(x) - 1))) %>% unlist(),
		Order = records$Order,
		#across(c(L, M, U), ~ replace(.x, Order < 1200, NA)),
	) %>% ggplot(aes(Order)) +
	geom_ribbon(aes(ymin = Target, ymax = U, fill = Order > 1200, alpha = Order > 1200)) +
	geom_ribbon(aes(ymax = Target, ymin = L, fill = FALSE), alpha = .1) +
	geom_line(aes(y = M), color = 'blue') +
	geom_line(aes(y = Target),color = 'darkred') +
	geom_point(aes(y = Target),color = 'darkred') +
	geom_vline(xintercept = 1200, linetype = 'dashed', alpha = .8) +
	theme_minimal() +
	scale_x_continuous(trans = 'log', labels = round, breaks = function(x) seq(0, log(tot_records), length.out = 10) %>% exp()) +
	scale_fill_viridis_d(labels = c('Resolved uncertainty', 'Residual uncertainty')) +
	guides(alpha = 'none') +
	scale_alpha_discrete(range = c(.1, .5)) +
	labs(x = 'Records', y = 'Cum. positive matches', fill = NULL, alpha = NULL)


########################

samples <- read_rds('Sessions/Session2/Samples/6.Samples_rep4_2021-07-12T15.09.48.rds')

records <- records %>%
	arrange(desc(Pred_Med)) %>%
	select(Order, starts_with('Rev_'), ID) %>%
	mutate(
		Target = coalesce_labels(records, c('Rev_manual', 'Rev_prediction',
																				'Rev_prediction_new', 'Rev_previous'))
	)

samples <- arrange(samples, match(ID, records$ID)) %>% select(-ID)

preds <- samples %>% as.matrix %>% {
	m <- .
	rbinom(n = length(m), prob = m, size = 1) %>% matrix(ncol = ncol(m))
}

preds[!is.na(records$Target),] <- as.numeric(na.omit(records$Target) == 'y')

tot_records = nrow(records)

uncertain_zone <- (preds %>% apply(1, cumsum)) %>%
	apply(1, quantile, c(.025, .5, .975)) %>% t %>% as.data.frame() %>%
	setNames(c('L', 'M', 'U'))

a <- parRapply(preds, quantile, c(.025, .5, .975), cl = cl) %>% t %>% as.data.frame() %>% setNames(c('L', 'M', 'U'))

uncertain_zone %>%
	trasmute(
		Target = cumsum(Target %in% 'y'),
		NewPos = tapply(Target, Target, function(x) c(x[1], rep(NA, length(x) - 1))) %>% unlist(),
		Order = records$Order,
		#across(c(L, M, U), ~ replace(.x, Order < 1200, NA)),
	) %>% ggplot(aes(Order)) +
	geom_ribbon(aes(ymin = L, ymax = U), alpha = .1, color = 'steelblue') +
	geom_line(aes(y = M), color = 'blue') +
	geom_line(aes(y = Target), color = 'darkred') +
	geom_point(aes(y = Target), color = 'darkred') +
	geom_vline(xintercept = 1200, linetype = 'dashed', alpha = .8) +
	theme_minimal() +
	scale_x_continuous(trans = 'log', labels = round, breaks = function(x) seq(0, log(tot_records), length.out = 10) %>% exp()) +
	scale_fill_viridis_d(labels = c('Resolved uncertainty', 'Residual uncertainty')) +
	guides(alpha = 'none') +
	labs(x = 'Records', y = 'Cum. positive matches', fill = NULL, alpha = NULL)


#################################
local({

	records <- arrange(records, desc(Pred_Low))

	tictoc::tic('predictions')
	preds_lo <- posterior_predict(mod_lo, newdata = records, nsamples = 2000, cores = 8)
	tictoc::toc()

	tot_records = nrow(records)

	tictoc::tic('preds_lo transpose')
	preds <- preds_lo# %>% t
	tictoc::toc()

	tictoc::tic('uncertainty removal')

	preds[,!is.na(records$Target)] <- as.numeric(na.omit(records$Target) == 'y')
	tictoc::toc()
	browser()
	tictoc::tic('cumsum')
	uncertain_zone <- (preds %>% apply(2, cumsum))
	tictoc::toc()

	tictoc::tic('quantiles')
	uncertain_zone <- apply(uncertain_zone, 2, quantile, c(.025, .5, .975)) %>% t %>% as.data.frame() %>%
		setNames(c('L', 'M', 'U'))
	tictoc::toc()

	uncertain_zone %>%
		transmute(
			Target = cumsum(records$Target %in% 'y'),
			NewPos = tapply(Target, Target, function(x) c(x[1], rep(NA, length(x) - 1))) %>% unlist(),
			Order = records$Order,
			#across(c(L, M, U), ~ replace(.x, Order < 1200, NA)),
		) %>%
		arrange(desc(records$Pred_Lo)) %>%
		ggplot(aes(Order)) +
		geom_ribbon(aes(ymin = L, ymax = U), alpha = .1, color = 'steelblue') +
		geom_line(aes(y = M), color = 'blue') +
		geom_line(aes(y = Target), color = 'darkred') +
		geom_point(aes(y = Target), color = 'darkred') +
		geom_vline(xintercept = 1200, linetype = 'dashed', alpha = .8) +
		theme_minimal() +
		scale_x_continuous(trans = 'log', labels = round, breaks = function(x) seq(0, log(tot_records), length.out = 10) %>% exp()) +
		scale_fill_viridis_d(labels = c('Resolved uncertainty', 'Residual uncertainty')) +
		guides(alpha = 'none') +
		labs(x = 'Records', y = 'Cum. positive matches', fill = NULL, alpha = NULL)
})


a %>%
	mutate(
		Target = cumsum(records$Target %in% 'y'),
		across(c(Target, L, M, U), ~ tapply(.x, .x, function(x) c(x[1], rep(NA, length(x) - 1))) %>% unlist()),
		max = coalesce(Target, L, M, U),
		Order = records$Pred_Low
	) %>% filter(!is.na(max)) %>%
	mutate(
		across(c(Target, L, M, U), ~ sapply(1:length(.x), function(i) {
			if (is.na(.x[i])) max(.x[1:i], na.rm = T) else .x[i]
		}))
	) %>% ggplot(aes(Order)) +
	geom_ribbon(aes(ymin = L, ymax = U), alpha = .1, color = 'steelblue') +
	geom_line(aes(y = M), color = 'blue') +
	geom_line(aes(y = Target), color = 'darkred') +
	geom_point(aes(y = Target), color = 'darkred') +
	#geom_vline(xintercept = 1200, linetype = 'dashed', alpha = .8) +
	theme_minimal() +
	scale_x_continuous(trans = 'log', labels = round, breaks = function(x) seq(0, log(tot_records), length.out = 10) %>% exp()) +
	scale_fill_viridis_d(labels = c('Resolved uncertainty', 'Residual uncertainty')) +
	guides(alpha = 'none') +
	labs(x = 'Records', y = 'Cum. positive matches', fill = NULL, alpha = NULL)
