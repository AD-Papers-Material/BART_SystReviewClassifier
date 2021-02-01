
# Setup -------------------------------------------------------------------

options(java.parameters = "-Xmx12g")

if (!('librarian' %in% installed.packages())) install.packages('librarian')

library(librarian)
shelf(dplyr, stringr, glue, readr, readxl, lubridate, Matrix, igraph, pbapply,
			pbmcapply, rpart, bartMachine, tm, patchwork, ggplot2, ggrepel, RLesur/crrri,
			patchwork)

# Packages required but not loaded
required.pkgs <- setdiff(c('purrr', 'WriteXLS', 'tictoc', 'tidyr', 'arm',
													 'parallel', 'jsonlite', 'rentrez',
													 'wosr'), installed.packages())

if (length(required.pkgs) > 0) install.packages(required.pkgs)

### Uncomment if mclapply fails on windows
# mclapply <- lapply
# pbmclapply <- pblapply


if (bart_machine_num_cores() != parallel::detectCores()) {

	message('Using ', parallel::detectCores(), ' cores')
	set_bart_machine_num_cores(parallel::detectCores())
	options(mc.cores = parallel::detectCores())
}

# Easier to use than !(a %in% b)
'%nin%' <- Negate('%in%')


# Nicer output than scales::percent()
percent <- function(x) {
	sapply(x, function(x) {
		if (!is.na(x)) {
			if (abs(x * 100) < 1) {
				sprintf('%s%%', signif(x * 100, 2))
			} else {
				sprintf('%s%%', signif(x * 100, 3))
			}
		} else NA
	})
}

today <- function() as_date(Sys.time())
now <- function() Sys.time()

# Tool to grab XHR messages from dynamic websites
get_website_resources <- function(url, url_filter = '.*', type_filter = '.*',
																	wait_for = 20,
																	n_of_resources = NULL, interactive = F) {

	crrri::perform_with_chrome(function(client) {
		Fetch <- client$Fetch
		Page <- client$Page

		if (interactive) client$inspect()

		out <- new.env()

		out$results <- list()
		out$resolve_function <- NULL

		out$pr <- promises::promise(function(resolve, reject) {
			out$resolve_function <- resolve

			Fetch$enable(patterns = list(list(urlPattern="*", requestStage="Response"))) %...>% {
				Fetch$requestPaused(callback = function(params) {

					if (str_detect(params$request$url, url_filter) & str_detect(params$resourceType, type_filter)) {

						Fetch$getResponseBody(requestId = params$requestId) %...>% {
							resp <- .

							if (resp$body != '') {
								if (resp$base64Encoded) resp$body = jsonlite::base64_dec(resp$body) %>% rawToChar()

								body <- list(list(
									url = params$request$url,
									response = resp
								)) %>% setNames(params$requestId)

								#str(body)

								out$results <- append(out$results, body)

								if (!is.null(n_of_resources) & length(out$results) >= n_of_resources) out$resolve_function(out$results)
							}

						}
					}

					Fetch$continueRequest(requestId = params$requestId)
				})
			} %...>% {
				Page$navigate(url)
			} %>% crrri::wait(wait_for) %>%
				then(~ out$resolve_function(out$results))

		})

		out$pr$then(function(x) x)
	}, timeouts = max(wait_for + 3, 30), cleaning_timeout = max(wait_for + 3, 30))
}


# A file path friendly lubridate::now()
safe_now <- function() str_replace_all(now(), c(' ' = 'T', ':' = '.'))


# Article data management -------------------------------------------------

clean_date_filter_arg <- function(year_query, cases,
																	arg_in_query_test = NULL, query = NULL) {

	if (class(year_query) %nin% c('NULL', 'character') | length(year_query) > 1) {
		stop('Year filter query should be a single character string or NULL')
	}

	if (is.character(year_query)) {

		year_query <- str_remove_all(year_query, '\\s+|^\\(|\\)$')

		if (!is.null(arg_in_query_test) & !is.null(query)) {
			if (str_detect(query, arg_in_query_test)) {
				warning('Year filter already in query. The query will be used')
				return(NULL)
			}
		}

		if (str_detect(year_query, '^\\d{4}-\\d{4}$')) { # range

			year_piece <- str_split(year_query, '-') %>% unlist

			if (year_piece[2] < year_piece[1]) warning('Years\' order seems wrong, please check it.')

			year_query <- glue(cases$range)

		} else if (str_detect(year_query, '^(<|<=|>=|>)\\d{4}$')) { # boundary

			pieces <- str_split(year_query, '\\b') %>% unlist
			comparator <- pieces[1]
			year_piece <- as.numeric(pieces[2])

			year_query <- switch(
				comparator,
				'>' = glue(cases$gt),
				'>=' = glue(cases$ge),
				'<=' = glue(cases$le),
				'<' = glue(cases$lt)
			)
		} else if (str_detect(year_query, '^\\d{4}$')) {

			year_piece <- year_query
			year_query <- glue(cases$eq)

		} else {
			stop('Year filter query is malformed. Possible patterns are:
	gt: > Year;
	ge: >= Year;
	lt: < Year;
	le: <= Year;
	eq: Year;
	range: Year1 - Year2')
		}
	}

	year_query

}

search_wos <- function(query, year_query = NULL, additional_fields = NULL,
											 default_field = 'TS', api_key = options('wos_api_key'),
											 parallel = T, parse_query = T, ...) {

	message('Searching WOS...')

	if (parallel) { ## Use mclapply which is faster
		pull_records <- function (query, editions = c("SCI", "SSCI", "AHCI", "ISTP",
																									"ISSHP", "BSCI", "BHCI", "IC", "CCR", "ESCI"), sid = auth(Sys.getenv("WOS_USERNAME"),
																																																						Sys.getenv("WOS_PASSWORD")), ...)
		{
			parse_wos <- function (all_resps)
			{
				pbmcapply::pbmclapply(all_resps, wosr:::one_parse)
			}

			qr_out <- wosr:::query_wos(query, editions = editions, sid = sid,
																 ...)
			if (qr_out$rec_cnt == 0) {
				dfs <- unique(schema$df)
				wos_unenforced <- vector("list", length = length(dfs))
				names(wos_unenforced) <- dfs
			}
			else {
				message("- fetching records")
				all_resps <- wosr:::download_wos(qr_out, ...)
				all_resps <- all_resps[vapply(all_resps, length, numeric(1)) >
															 	1]
				message("- parsing results")
				parse_list <- parse_wos(all_resps)
				df_list <- wosr:::data_frame_wos(parse_list)
				wos_unenforced <- wosr:::process_wos_apply(df_list)
			}
			wos_data <- wosr:::enforce_schema(wos_unenforced)
			wosr:::append_class(wos_data, "wos_data")
		}
	} else pull_records <- wosr::pull_wos


	if (parse_query) {
		query <- str_squish(query)

		if (str_detect(query, '^\\w+ ?= ?')) {
			pieces <- str_split(query, '( ?AND ?)?\\w{2} ?= ?') %>% unlist() %>%
				str_squish() %>% str_subset('^$', negate = T)

			fields <- str_extract_all(query, '\\w{2} ?= ?') %>% unlist() %>%
				str_squish() %>% str_remove(' ?= ?')

			query <- setNames(pieces, fields)

		} else {
			query <- setNames(glue('{query}'), default_field)
		}

		if (is.character(year_query)) {

			year_query <- clean_date_filter_arg(year_query, cases = list(
				gt = '{year_piece + 1}-{year(today())}', ge = '{year_piece}-{year(today())}',
				eq = '{year_piece}-{year_piece}', le = '1985-{year_piece}',
				range = '{year_piece[1]}-{year_piece[2]}', lt = '1985-{year_piece - 1}'),
				arg_in_query_test = 'PY ?=', query = query)

			additional_fields <- c(
				setNames(year_query, 'PY'),
				additional_fields[names(additional_fields) %nin% 'PY']
			)
		}

		query <- c(query, additional_fields)

		query <- paste(
			glue('{names(query)} = ({query})'),
			collapse = ' AND '
		)
	}

	records_list <- tryCatch(
		pull_records(query, sid = api_key, ...),
		error = function(e) stop(e, glue("\n\nquery: {query}"))
	)

	records <- records_list$publication %>%
		transmute(Order = 1:n(), ID = ut, Title = title, Abstract = abstract, DOI = doi,
							Journal = journal, N_citations = tot_cites,
							Published = format(ymd(date),'%b %Y'), Source = 'WOS',
							Source_type = 'API')

	additional_infos <- list(
		authors = records_list$author %>% group_by(ID = ut) %>%
			summarise(Authors = paste(display_name, collapse = '; ')),
		topics = records_list$jsc %>% group_by(ID = ut) %>%
			summarise(Topic = paste(jsc, collapse = '; ')),
		art_type = records_list$doc_type %>% group_by(ID = ut) %>%
			summarise(Article_type = paste(doc_type, collapse = '; ')),
		auth_keys = records_list$keyword %>% group_by(ID = ut) %>%
			summarise(Author_keywords = paste(keyword, collapse = '; ')),
		keys = records_list$keywords_plus %>% group_by(ID = ut) %>%
			summarise(Keywords = paste(keywords_plus, collapse = '; '))
	)

	for (info in additional_infos) records <- left_join(records, info, by = 'ID')

	records <- mutate(records,
										across(where(is.character), ~ replace(.x, .x == '', NA) %>%
													 	str_replace_all(c(' +;' = ';', '["\']+' = ' ')) %>%
													 	str_squish(.x)))

	message('...found ', nrow(records), ' records.')

	records
}

parse_medline <- function(entries) {
	entries <- entries %>%
		str_remove_all('\\r') %>%
		str_replace_all('\\n\\s\\s+', ' ') %>%
		str_trim() %>%
		str_split('\\n+(?=PMID-)') %>% unlist

	tags <- c('TI', 'BTI', 'AB', 'JT', 'TA', 'DP')
	info <- lapply(tags, function(tag) {
		str_extract(entries, sprintf('(?<=\\n)%s *- .+', tag)) %>% str_remove('[A-Z]+ *- ')
	}) %>% setNames(tags) %>% bind_cols()

	tags <- c('FAU', 'PT', 'MH', 'OT')
	info <- cbind(info, lapply(tags, function(tag) {
		str_extract_all(entries, sprintf('(?<=\\n)%s *- .+', tag)) %>% sapply(function(x) str_remove(x, '[A-Z]+ *- ') %>% paste0(collapse = '; '))
	}) %>% setNames(tags) %>% bind_cols())

	tags <- c('LID', 'AID')
	info <- cbind(info, lapply(tags, function(tag) {
		str_extract(entries, sprintf('(?<=\\n)%s *- .+(?= \\[doi\\])', tag)) %>% str_remove('[A-Z]+ *- ')
	}) %>% setNames(tags) %>% bind_cols())

	info$PMID = paste0('PMID:', str_extract(entries, '(?<=PMID- )\\d+'))

	info %>% transmute(
		Order = 1:n(),
		ID = PMID, Title = ifelse(is.na(TI), BTI, TI),
		Abstract = AB, DOI = ifelse(is.na(LID), AID, LID),
		Authors = FAU, Journal = JT, Journal_short = TA,
		Article_type = PT, Mesh = MH, Author_keywords = OT, Published = DP,
		Source = 'Pubmed',
	) %>% mutate(
		across(where(is.character), ~ replace(.x, .x == '', NA) %>%
					 	str_replace_all(c(' +;' = ';', '["\']+' = ' ')) %>%
					 	str_squish(.x))
	)
}


search_pubmed <- function(query, year_query = NULL, additional_fields = NULL,
													api_key = options('ncbi_api_key'),
													record_limit = numeric(),
													...) {

	message('Searching Pubmed...')

	if (is.null(api_key)) warning('NCBI API key is not set.')

	query <- str_squish(query)

	year_query <- clean_date_filter_arg(year_query, cases = list(
		gt = '{year_piece + 1}[PDAT]:{year(today())}[PDAT]',
		ge = '{year_piece}[PDAT]:{year(today())}[PDAT]',
		eq = '{year_piece}[PDAT]:{year_piece}[PDAT]', le = '1000[PDAT]:{year_piece}[PDAT]',
		range = '{year_piece[1]}:[PDAT]{year_piece[2]}[PDAT]',
		lt = '1000[PDAT]:{year_piece - 1}[PDAT]'),
		arg_in_query_test = '[PDAT]', query = query)

	year_query <- glue('({year_query})') # adding parenthesis around the dates

	if (!is.null(additional_fields)) {
		additional_fields <- paste(glue('({additional_fields})[{names(additional_fields)}]'), collapse = ' AND ')
	}

	query <- paste(query, year_query, additional_fields, collapse = ' AND ') %>% str_squish()

	res <- rentrez::entrez_search(db = "pubmed", term = query, retmax = 0,
																api_key = api_key, use_history = T)

	total_count <- min(res$count, record_limit)

	message('- fetching records')

	steps <- floor((total_count - 1) / min(total_count, 200))

	# ~ 20x faster than pubmedR::pmApiRequest plus xml parsing
	records <- pbmclapply(0:steps, function(step) {
		#print(paste(step * 200))
		try(rentrez::entrez_fetch(db = "pubmed", web_history = res$web_history,
															retstart = step * 200, retmax = min(200, total_count - step * 200),
															rettype = 'medline', parsed = F,
															api_key = options('ncbi_api_key')), silent = T)
	})

	failed.steps <- sapply(records, function(x) class(x) == 'try-error') %>% which

	if (length(failed.steps) > 0) {
		message(paste('- repeating', length(failed.steps), 'failed fetch tentatives'))
		refetched <- pblapply(failed.steps, function(step) {
			rentrez::entrez_fetch(db = "pubmed", web_history = res$web_history,
														retstart = step * 200, retmax = 200, rettype = 'medline', parsed = F, api_key = options('ncbi_api_key'))
		})

		records[failed.steps] <- refetched
	}

	message('- parsing results')

	records <- parse_medline(records %>% unlist() %>% paste(collapse = '\\n\\n')) %>%
		mutate(Source_type = 'API')

	message('...found ', nrow(records), ' records.')

	records

}

search_ieee <- function(query, year_query = NULL, additional_fields = NULL,
												api_key = options('ieee_api_key')[[1]], allow_web_scraping = T,
												file_name = glue('IEEE_{safe_now()}'), save = T,
												wait_for = 20, record_limit = NULL) {
	message('Searching IEEE...')

	if (!is.null(additional_fields) & length(additional_fields) > 0) {

		if (!is.list(additional_fields)) stop('additional_fields must be a list.')

		additional_fields <- lapply(additional_fields, function(el) {
			if (el == TRUE) 'true' else if (el == FALSE) 'false'
			else el
		})
	}

	if (is.null(api_key[[1]])) {
		warning('IEEE API key is not set, defaulting to webscraping.')

		if (!allow_web_scraping) stop('If API key is not present web scraping must be allowed.')

		default_fields <- list(
			rowsPerPage = 100
		)

		default_fields[names(additional_fields)] <- NULL

		additional_fields <- c(default_fields, additional_fields)

		additional_fields$rowsPerPage <- min(as.numeric(additional_fields$rowsPerPage), record_limit)

		if ('ranges' %nin% names(additional_fields) & !is.null(year_query)) {
			year_arg <- clean_date_filter_arg(year_query, cases = list(
				gt = '{year_piece + 1}_{year(today())}',
				ge = '{year_piece}_{year(today())}',
				eq = '{year_piece}_{year_piece}',
				range = '{year_piece[1]}_{year_piece[2]}',
				le = '1800_{year_piece}',
				lt = '1800_{year_piece - 1}'))

			additional_fields$ranges <- glue('{year_arg}_Year')
		}

		endpoint <-  httr::parse_url('https://ieeexplore.ieee.org/search/searchresult.jsp?')

		endpoint$query <- c(queryText = query, additional_fields) %>% lapply(str_squish)

		url <- httr::build_url(endpoint)

		message('- fetching records')
		response <- get_website_resources(url = url, url_filter = 'rest/search',
																			type_filter = 'XHR', wait_for = wait_for)

		if (length(response) == 0) {
			stop('No results were scraped. Try using a longer wait_time to allow for more time to load results.')
		}

		response <- jsonlite::fromJSON(response[[1]]$response$body)

		records <- response$records

		if (response$totalPages > 1) {

			if (response$totalPages > 100) warning('Only results up to page 100 are available')

			other_pages <- pblapply(2:min(response$totalPages, 100), function(page) {

				endpoint$query$pageNumber <- page

				if (page * endpoint$query$pageNumber > record_limit) {
					endpoint$query$rowsPerPage <- record_limit - ((page - 1) * endpoint$query$rowsPerPage)
				}

				url <- httr::build_url(endpoint)

				response <- get_website_resources(url = url, url_filter = 'rest/search',
																					type_filter = 'XHR', wait_for = wait_for)

				response$records
			})

			records <- bind_rows(records, other_pages)
		}

		records <- response$records %>%
			transmute(
				Order = 1:n(),
				ID = paste0('IEEE:', articleNumber),
				Title = articleTitle,
				DOI = doi, URL = paste0('https://ieeexplore.ieee.org/document/', articleNumber),
				Authors = authors %>% sapply(function(df) paste(df$normalizedName, collapse = '; ')),
				Journal = publicationTitle,
				Article_type = str_remove(`contentType`, '(IEEE|OUP) '), #there may be more...
				N_citations = citationCount,
				Published = publicationDate
			)

		#if (!is.null(record_limit)) records <- head(records, record_limit)

	}
	else {
		default_fields <- list(
			max_records = 200
		)

		default_fields[names(additional_fields)] <- NULL

		additional_fields <- c(default_fields, additional_fields)

		additional_fields$max_records <- min(as.numeric(additional_fields$max_records), record_limit)

		if (all(c('start_year', 'end_year') %nin% names(additional_fields)) & !is.null(year_query)) {
			year_arg <- clean_date_filter_arg(year_query, cases = list(
				gt = '{year_piece + 1}_x',
				ge = '{year_piece}_x',
				eq = '{year_piece}_{year_piece}',
				range = '{year_piece[1]}_{year_piece[2]}',
				le = 'x_{year_piece}',
				lt = 'x_{year_piece - 1}')) %>% str_split('_') %>%
				unlist() %>% setNames(c('start_year', 'end_year')) %>%
				Filter(f = function(el) el != 'x')

			additional_fields <- c(additional_fields, year_arg)
		}

		endpoint <-  'http://ieeexploreapi.ieee.org/api/v1/search/articles'

		query <- c(querytext = query, additional_fields,
							 apikey = api_key, format = 'json') %>% lapply(str_squish)

		message('- fetching records')

		response <- httr::GET(url = endpoint, query = query)

		if (response$status_code != 200) {
			stop('Error fetching results, with code', response$status_code, '

					 ', httr::content(response, 'text'))
		}

		results <- httr::content(response, 'text') %>% jsonlite::fromJSON()

		records <- results$articles

		max_records <- additional_fields$max_records

		if (results$total_records > max_records) {
			total_count <- min(results$total_records, record_limit)

			steps <- floor((total_count - 1) / min(total_count, max_records))

			other_pages <- pblapply(1:steps, function(step) {
				print(paste(step * max_records + 1))

				query$start_record <- step * max_records + 1
				query$max_records <- min(max_records, total_count - step * max_records)

				response <- httr::GET(url = endpoint, query = query)

				if (response$status_code != 200) {
					stop('Error fetching results, with code', response$status_code, '

					 ', httr::content(response, 'text'))
				}
				else {
					results <- httr::content(response, 'text') %>% jsonlite::fromJSON()

					results$articles
				}
			}) %>% bind_rows()

			records <- bind_rows(records, other_pages)
		}

		records <- records %>%
			transmute(
				Order = 1:n(),
				ID = paste0('IEEE:', publication_number),
				Title = title,
				Abstract = abstract,
				DOI = doi,
				URL = abstract_url,
				Journal = publication_title,
				Article_type = content_type,
				N_citations = citing_paper_count,
				Published = publication_date,
			)
	}

	message('- fetching individual article data')

	article_data <- pbmclapply(records$URL, function(URL) {
		if (!str_detect(URL, fixed('https://ieeexplore.ieee.org/document/'))) {
			return(NULL)
		}

		data <- read_file(URL) %>%
			str_extract('(?<=xplGlobal\\.document\\.metadata=).+') %>%
			str_remove(';$') %>% jsonlite::fromJSON()

		Keys <- data$keywords %>%
			group_by(type = case_when(
				str_detect(type, 'MeSH') ~ 'Mesh',
				str_detect(type, 'Author') ~ 'Author',
				T ~ 'IEEE'
			)) %>%
			summarise(kwd = paste(unlist(kwd), collapse = '; '))

		Keys <- setNames(as.list(Keys$kwd), Keys$type)

		ret <- bind_cols(
			URL = URL,
			Keywords = Keys$IEEE,
			Mesh = Keys$Mesh,
			Author_keywords = Keys$Author,
		)

		if ('Authors' %nin% names(records)) {

			ret$Authors <- data$authors %>% as.data.frame() %>%
				transmute(
					firstName = if (exists('firstName')) firstName else NA,
					lastName = if (exists('lastName')) lastName else NA,
					across(c(firstName, lastName), ~ replace(.x, is.na(.x), ''))
				) %>%
				with(paste(
					str_replace_all(firstName, c(
						'[^\\w]+' = ' ',
						'\\b(\\w)\\w*' = '\\1.')),
					lastName,
					collapse = '; '))

		}

		ret
	}) %>% bind_rows()

	records <- left_join(records, article_data, by = 'URL') %>% mutate(
		across(where(is.character), ~ replace(.x, .x == '', NA) %>%
					 	str_replace_all(c(' +;' = ';', '["\']+' = ' ')) %>%
					 	str_squish()),
		Source = 'IEEE', Source_type = 'API'
	) %>% select(Order, ID, Title, Abstract, DOI, URL, Authors, Journal,
							 Article_type, Author_keywords, Keywords, Mesh, N_citations,
							 Published, Source, Source_type)

	message('...found ', nrow(records), ' records.')

	records
}


read_bib_files <- function(files) {

	pblapply(files, function(file) {

		if (str_detect(file, '(parsed|API)\\.csv')) {  # no parsing necessary
			message('Reading ', basename(file), '...')

			return(read_csv(file))
		}

		message('Parsing ', basename(file), '...')

		type <- NULL

		if (str_detect(file, '\\.(nbib|txt)$')) {
			entries <- read_file(file)

			if (str_detect(entries, 'PMID-')) type <- 'nbib'

		} else if (str_detect(file, '\\.(xlsx?|csv)$')) {
			entries <- if (str_detect(file, '\\.csv$')) read_csv(file) else read_excel(file)

			if ('UT (Unique WOS ID)' %in% colnames(entries)) type <- 'wos'
			else if ('IEEE Terms' %in% colnames(entries)) type <- 'ieee'
		}

		if (is.null(type)) {
			warning('Format not recognized')
			return(NULL)
		}

		if (type == 'nbib') {

			parse_medline(entries) %>%
				mutate(Source_type = 'parsed')
		}

		else if (type == 'wos') {

			entries %>% transmute(
				Order = 1:n(),
				ID = `UT (Unique WOS ID)`,
				Title = `Article Title`,
				Abstract, DOI,
				Authors = `Author Full Names`,
				Journal = `Source Title`,
				Journal_short = `Journal ISO Abbreviation`,
				Article_type = `Document Type`,
				Author_keywords = `Author Keywords`,
				Keywords = `Keywords Plus`,
				Topic = `WoS Categories`,
				N_citations = `Times Cited, All Databases`,
				Published = paste(`Publication Date`, `Publication Year`),
				PMID = `Pubmed Id`,
				Source = 'WOS',
				Source_type = 'parsed',
			) %>% mutate(
				across(where(is.character), ~ replace(.x, .x == '', NA) %>%
							 	str_replace_all(c(' +;' = ';', '["\']+' = ' ')) %>%
							 	str_squish(.x))
			)
		}

		else if (type == 'ieee') {

			entries %>% transmute(
				Order = 1:n(),
				ID = paste0('IEEE:', str_remove(`PDF Link`, fixed('https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber='))),
				Title = `Document Title`,
				Abstract, DOI, URL = `PDF Link`,
				Authors, Journal = `Publication Title`,
				Author_keywords = `Author Keywords`,
				Keywords = cbind(`IEEE Terms`, `INSPEC Controlled Terms`, `INSPEC Non-Controlled Terms`) %>%
					apply(1, function(x) if (any(!is.na(x))) paste(na.omit(x), collapse = ';') else NA),
				Mesh = Mesh_Terms,
				Article_type = str_remove(`Document Identifier`, 'IEEE '),
				N_citations = `Article Citation Count`,
				Published = `Online Date`,
				Source = 'IEEE',
				Source_type = 'parsed',
			) %>% mutate(
				across(where(is.character), ~ replace(.x, .x == '', NA) %>%
							 	str_replace_all(c(' +;' = ';', '["\']+' = ' ')) %>%
							 	str_squish(.x))
			)
		}
	}) %>% setNames(basename(files))
}

join_sources <- function(source.list) {
	lapply(source.list, function(source) {
		source %>% transmute(
			Order,
			DOI, ID, Title, Abstract, Authors, Year = Published %>% str_extract('\\d{4}') %>% as.numeric(),
			Journal = if (exists('Journal')) Journal else NA,
			Journal_short = if (exists('Journal_short')) Journal_short else NA,
			Keywords = if (exists('Keywords')) Keywords else NA,
			Author_keywords = if (exists('Author_keywords')) Author_keywords else NA,
			Mesh = if (exists('Mesh')) Mesh else NA,
			Article_type,
			N_citations = if (exists('N_citations')) N_citations else NA,
			Source, Source_type, FileID
		)
	}) %>% bind_rows() %>%
		mutate(
			Keywords = cbind(Keywords, Author_keywords) %>%
				apply(1, function(x) if (any(!is.na(x))) paste(na.omit(x), collapse = ';') else NA) %>% str_to_lower,
			Author_keywords = NULL
		) %>%
		fix_duplicated_records() %>%
		mutate(
			Keywords = str_split(Keywords, '\\s*;\\s*') %>% sapply(function(x) {
				str_remove(x, '^[\\*\\-"\\\']+ *') %>% str_remove(' *[\\*\\-"\\\']+ *$') %>%
					unique() %>% paste(collapse = '; ')
			})
		) %>%
		arrange(Order)
}


update_annotation_file <- function(sources = NULL, source_folder = 'Records', recursive = T, prev_records = NULL, out_type = c('xlsx', 'csv')) {

	out_type <- match.arg(out_type)

	if (is.null(sources) & is.null(source_folder)) {
		stop('Either parsed sources or sources path are needed')
	}

	if (is.null(sources) & !is.null(source_folder)) {
		message('- parsing sources...')
		sources <- read_bib(list.files(source_folder, full.names = T, recursive = recursive))
	}

	message('- joining sources...')
	joined_sources <- join_sources(sources) %>%
		mutate(
			Rev_title = NA,
			Rev_abstract = NA,
			Rev_text = NA,
			.before = DOI
		)

	message(": ", nrow(joined_sources), ' unique records')

	if (!is.null(prev_records) & file.exists(prev_records)) {

		if (str_detect(prev_records, '\\.xlsx?$')) {
			old_sources <- read_excel(prev_records)
		} else old_sources <- read_csv(prev_records)

		joined_sources <- joined_sources %>% filter(!(ID %in% old_sources$ID))

		message("(", nrow(joined_sources), ' new records)')

		joined_sources <- full_join(
			old_sources, joined_sources
		) %>%
			mutate(
				Parent_file = prev_records,
			) %>%
			fix_duplicated_records()

	}

	joined_sources <- joined_sources %>% arrange(Order)

	message('- saving records...')

	file <- file.path('Annotations', paste0('Records_', safe_now(), '.', out_type))

	if (out_type == 'xlsx') {
		WriteXLS::WriteXLS(joined_sources, ExcelFileName = file)
	} else write_csv(joined_sources, file = file)

	invisible(joined_sources)
}

fix_duplicated_records <- function(Records) {
	Records <- Records %>% mutate(
		UID = str_to_lower(Title) %>% str_remove_all('[^\\w\\d\\s]+')
	)

	dup_recs <- Records$UID[duplicated(Records$UID) | duplicated(Records$DOI)]

	unique_sources <- Records %>% filter(!(UID %in% dup_recs))
	dup_sources <- Records %>% filter(UID %in% dup_recs)

	dup_sources <- dup_sources %>%
		group_by(UID) %>%
		summarise(
			Order = min(Order),
			across(any_of(c('Title', 'Abstract', 'Authors', 'Journal', 'Journal_short', 'Year',
											"Pred_delta", "Pred_Med", "Pred_Low", "Pred_Up")), ~ last(na.omit(.x))),
			across(any_of(c('ID', 'DOI', 'Mesh', 'Article_type', 'Source', 'Source_type', 'FileID',
											"Rev_title", "Rev_abstract", "Rev_prediction",
											"Rev_text", "Predicted_label")), ~ na.omit(.x) %>% unique() %>% paste(collapse = '; ')),
			Keywords = Keywords %>% str_split('; ') %>% unlist() %>% na.omit() %>% unique() %>%
				purrr::keep(~ str_length(.x) > 0) %>%
				paste(collapse = '; '),
			N_citations = suppressWarnings(na.omit(N_citations) %>% max(na.rm = T) %>% purrr::modify_if(~ !is.finite(.x), ~ NA))
		)

	bind_rows(unique_sources, dup_sources) %>% select(-UID)
}

summarise_by_source <- function(file) {
	data <- read_excel(file)

	sources <- data$Source %>% str_split(., '; ') %>% unlist() %>% table()

	c(setNames(as.vector(sources), names(sources)), Total = nrow(data))
}



# NLP ---------------------------------------------------------------------

lemmatize <- function(text.vec, dict = lexicon::hash_lemmas, separator = '_tagseparator_') {
	dict <- setNames(lexicon::hash_lemmas$lemma, lexicon::hash_lemmas$token)

	terms <- paste(text.vec, separator, collapse = ' ')
	terms <- gsub(sprintf(' *%s$', separator), '', terms, perl = T) %>%
		str_split('\\b') %>% unlist
	terms <- terms[!(terms %in% c('', ' '))]

	terms.lower <- tolower(terms)

	output <- ifelse(
		terms.lower %in% names(dict),
		dict[terms.lower], terms
	) %>%
		paste(collapse = ' ') %>%
		str_split(sprintf(' *%s *', separator)) %>% unlist

	output <- gsub('\\s+', ' ', output, perl = T)
	output <- gsub('^\\s+|\\s+$', '', output, perl = T)

	replace(output, output == 'NA', NA)


	# terms <- paste(text.vec, separator, collapse = ' ') %>%
	# 	str_remove(sprintf(' *%s$', separator)) %>%
	# 	str_split('\\b') %>% unlist %>%
	# 	str_subset('^ ?$', negate = T)
	#
	# terms.lower <- str_to_lower(terms)
	#
	# output <- ifelse(
	# 	terms.lower %in% names(dict),
	# 	dict[terms.lower], terms
	# ) %>%
	# 	paste(collapse = ' ') %>%
	# 	str_split(sprintf(' *%s *', separator)) %>% unlist %>%
	# 	str_squish()
	#
	# replace(output, output == 'NA', NA)
}


tokenize_text <- function(corpus) {

	message('- tokenizing text...')
	#tictoc::tic()

	stopwords <- stopwords("english")

	#  tictoc::tic()
	# corpus <- corpus %>% str_to_lower %>% # low case
	# 	str_replace_all('-', '_') %>%
	# 	removeWords(stopwords("english")) %>% # remove stopwords
	# 	str_replace_all(c(
	# 		"\'(s|re|t|d)?\\b" = '',
	# 		'_' = ' ',
	# 		'[^\\w\\d\\s]+' = ' ' # remove non letters/numbers/spaces
	# 	)) %>%
	# 	lemmatize() # lemmatize
	#  tictoc::toc()

	tictoc::tic()
	corpus <- tolower(corpus)
	corpus <- gsub('-', '_', corpus, fixed = T)
	corpus <- removeWords(corpus, stopwords("english"))
	corpus <- gsub("\'(s|re|t|d)?\\b", '', corpus, perl = T)
	corpus <- gsub('_',' ', corpus, fixed = T)
	corpus <- gsub('[^\\w\\d\\s]+', ' ', corpus, perl = T)
	corpus <- lemmatize(corpus)

	tictoc::toc()

	corpus
}

tokenize_authors <- function(corpus) {

	message('- tokenizing authors')
	tictoc::tic()

	ids = 1:length(corpus)

	with.comma <- str_detect(corpus, ',')

	corpus <- corpus %>% str_squish()

	output <- mclapply(1:length(corpus), function(i) {
		if (is.na(with.comma[i])) NA # No authors listed
		else if (with.comma[i] == TRUE) { # Pubmed or WOS style author list
			corpus[i] %>%
				str_remove_all('[^\\w ,;]') %>%
				str_replace_all('(?<=,)[ \\-\\w]+?(?:(?=;)|$)', function(x) {
					paste0(str_extract_all(x, '\\b\\w')[[1]], collapse = '')
				}) %>% str_replace_all(',', '_') %>% str_remove_all(' +')
		} else { # IEEE style author list
			corpus[i] %>%
				str_remove_all('[^\\w\\.;]') %>% # remove non letters and other characters
				str_replace_all('[^;]+(?:(?=;)|$)', function(x) { # extract names between ;
					str_replace(x, '([\\w \\.]+)\\.([\\w ]+)', '\\2_\\1') #use the rightmost dot to separate first and last names
				}) %>% str_remove_all('\\.')
		}
	}, mc.cores = 8) %>% unlist %>%
		str_replace_all('; *', ' ')

	tictoc::toc()

	output
}

tokenize_MESH <- function(mesh) {

	message('- tokenizing Mesh terms')
	tictoc::tic()

	output <- mesh %>% str_replace_all(c(' *; *' = ';', '[\\(\\)]' = '', '[ ,\\-]+' = '_', '&' = 'and')) %>%
		str_replace_all('(?:(?<=;)|^)[^;]+/[^;]+(?:(?=;)|$)', function(x) {
			x <- str_split(x, '/')[[1]]
			paste(c(x[1], paste(x[1],x[-1], sep = '.sh.')), collapse = ';')
		}) %>% str_replace_all(';', ' ') %>% str_squish()

	tictoc::toc()

	output
}


text_to_DTM <- function(corpus, min.freq = 20, ids = 1:length(corpus), freq.subset.ids = ids,
												included.pos = c('Noun', 'Verb', 'Adjective'),
												tokenize.fun = tokenize_text, add.ngrams = T, aggr.synonyms = T, n.gram.thresh = .5,
												syn.thresh = .9, label = 'TERM__', na.as.missing = T) {

	raw.corpus <- corpus
	order.ids <- 1:length(corpus)
	names(ids) <- order.ids

	if (!is.null(tokenize.fun)) {
		corpus <- tokenize.fun(corpus)
	}

	splitted.corpus <- corpus %>% str_split(' +')

	if (length(splitted.corpus) != length(ids)) stop('Number of documents and ids are different')

	excluded.pos <- lexicon::hash_grady_pos %>%
		mutate(pos = str_remove_all(pos, ' \\(.*')) %>%
		filter(!(pos %in% included.pos) & !(word %in% word[pos %in% included.pos])) # Keep terms that are ONLY associated to non relevant parts of speech

	message('- to long format...')
	tictoc::tic()

	corpus <- data.frame(
		term =  splitted.corpus %>% unlist,
		val = 1,
		ID = rep(order.ids, splitted.corpus %>% sapply(length))
	) %>% na.omit %>% distinct %>%
		mutate(
			val = replace(val, str_detect(term, '\\*'), 2),
			term = str_remove(term, '\\*')
		) %>%
		filter(!(term %in% excluded.pos$word))
	tictoc::toc()

	message('- removing rare terms...')
	frequent_terms <- corpus %>%
		filter(ID %in% order.ids[ids %in% freq.subset.ids]) %>%
		count(term, name = 'Freq') %>% # count term frequency, but only in relevant IDs
		filter(Freq >= min.freq) %>% pull(term) # create frequent terms list

	corpus <- corpus %>% filter(term %in% frequent_terms) %>% # filter out unfrequent terms
		arrange(ID, term, desc(val)) %>%
		distinct(ID, term, .keep_all = T) # Remove duplicate terms keeping the first of each occurrence (useful for Mesh data)

	message('- to wide format...')
	tictoc::tic()
	DTM <- tidyr::pivot_wider(corpus, id_cols = ID, names_from = term,
														names_prefix = label, values_from = val,
														values_fill = 0)

	tictoc::toc()

	if (add.ngrams) {
		message('- find non consecutive ngram...')
		tictoc::tic()

		DTM <- DTM.add_ngrams(DTM, min.sim = n.gram.thresh)

		tictoc::toc()
	}

	if (aggr.synonyms) {

		message('- find synonyms...')
		tictoc::tic()

		DTM <- DTM.aggr_synonyms(DTM, min.sim = syn.thresh)

		tictoc::toc()
	}

	message('- managing missings...')

	if (nrow(DTM) < length(raw.corpus)) { # Add documents with no content, ie. NAs
		missing_docs <- setdiff(order.ids, DTM$ID)

		DTM <- bind_rows(
			DTM,
			DTM[rep(1, length(missing_docs)),] %>% # add the missing documents using the first DTM row as template
				mutate(ID = missing_docs, across(c(-ID), ~ 0)) # set the term score to zero for all added documents
		) %>% arrange(ID)
	}

	if (na.as.missing) { # Put NA to terms for documents with no content, otherwise leave zero
		DTM <- DTM %>% mutate(across(-ID, ~ replace(.x, is.na(raw.corpus), NA)))
	}

	DTM %>% mutate(ID = ids[ID])
}

DTM.add_ngrams <- function(DTM, min.sim = .5) {
	mat <- as.matrix(DTM[,-1])

	mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices

	TTM <- (t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = T))) # Cosine similarity

	ngram.cliques <- graph_from_adjacency_matrix(as.matrix(TTM) >= min.sim, mode = 'undirected', diag = F) %>% # From TTM to undirected network
		max_cliques(min = 2) %>% lapply(names) # Extracting cliques

	if (length(ngram.cliques) > 0) {
		new_cols <- lapply(ngram.cliques, function(clique) { # For each clique
			new_col <- data.frame(
				as.numeric(DTM[, clique] %>% rowSums() == length(clique)) # TRUE if all words are present in the doc
			)

			colnames(new_col) <- paste0(str_extract(clique[1], '\\w+__'), str_remove(clique, '\\w+__') %>% paste(collapse = '._.'))

			new_col
		}) %>% bind_cols()

		DTM <- DTM %>% bind_cols(new_cols) # Add joined terms
	}
}

DTM.aggr_synonyms <- function(DTM, min.sim = .9) {

	mat <- as.matrix(DTM[,-1])

	mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices

	TTM <- (t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = T))) # Cosine similarity

	syn.components <- graph_from_adjacency_matrix(as.matrix(TTM) >= min.sim, mode = 'undirected', diag = F) %>% # From TTM to undirected network
		components() # Extracting connected subgraphs

	syn.components <- lapply(which(syn.components$csize > 1), function(i) {
		names(syn.components$membership)[syn.components$membership == i]
	})

	if (length(syn.components) > 0) {
		new_cols <- lapply(syn.components, function(component) { # For each component
			new_col <- data.frame(
				as.numeric(DTM[, component] %>% rowSums() > 0) # TRUE if at least one word is in the doc
			)

			colnames(new_col) <- paste0(str_extract(component[1], '\\w+__'), str_remove(component, '\\w+__') %>% paste(collapse = '.'))

			new_col
		}) %>% bind_cols()

		DTM <- DTM %>% select(-all_of(syn.components %>% unlist)) %>% # Remove single terms
			bind_cols(new_cols) # Add joined terms
	}
}

impute_edges <- function(template.mat, target.mat = NULL, a = 1, b = 1) {
	# DTM <- DTM %>% select(matches('ABSTR__'))
	#
	# mat <- as.matrix(DTM %>% na.omit)
	#
	# mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices
	#
	# TTM <- as.matrix((t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = T))) > .7) + 0 # Cosine similarity
	#
	# diag(TTM) <- 0

	elist <- tidyr::pivot_longer(data.frame(From = row.names(template.mat), template.mat), -From, values_to = 'Val', names_to = 'To') %>%
		mutate(across(c(From, To), factor), ord = as.numeric(From) + as.numeric(To), Val = Val) %>%
		filter(To != From) %>% arrange(ord, From, To) %>%
		group_by(ord) %>% slice_head(prop = .5) %>%
		ungroup()

	template.mat <- as(template.mat, "dgCMatrix")

	elist <- elist %>% mutate(
		Union = rowSums(template.mat[From,] | template.mat[To,]) - if_else(Val == 1, 2, 0),
		Inter = rowSums(template.mat[From,] & template.mat[To,]),
		Diff = Union - Inter,
		#R =  Inter / Union # Raw
		R =  (Inter + a) / (Union + a + b) # Beta mean
		#R = qbeta(.5, a + Inter, b + Diff) # Beta median
		#R = (Inter + a - 1) / (Inter + a + Diff + b - 2) # Beta mode
	)

	# posterios = P(R | edge) * P(edge) / P(R)

	rbind(
		count(elist %>% group_by(Val), Val, R) %>% mutate(Cond = T),
		count(elist, R) %>% mutate(Cond = F, Val = 3)
	) %>%
		na.omit %>%
		group_by(Val) %>%
		mutate(p = n/sum(n)) %>% ungroup %>% {
			df <- .
			df %>% #filter(., Val == 1) %>%
				mutate(
					p_data = sapply(R, function(x) {
						with(filter(df, Val == 3), p[R == x])
					}),
					prior = mean(elist$Val),
					post = (p * prior) / p_data,
					f_post = scales::percent(post)
				)

		}

}


# Tree model helpers ------------------------------------------------------

get_tree_rules <- function(tree, rule.as.text = T, eval.ready = F, mark.leaves = F) {

	mod_type <- if ('rpart' %in% class(tree)) 'rpart' else if ('party' %in% class(tree)) 'party' else stop('Only partikit::ctree() or rpart:rpart() models are supported')

	out <- capture.output(tree)
	out <- out[-(1:grep('1[^\\d]+root', out))]

	if (mod_type == 'party') {
		if (length(tree) == 1) return(data.frame(rule = character(0), id = numeric(0), depth = numeric(0)))

		rules <- tibble(
			rule = out[1:(length(out) - 3)] %>%
				str_remove('\\|\\s+'),
			id = str_extract(rule, '\\[\\d+\\]') %>% parse_number(),
			depth = str_count(rule, '\\|') + 1,
			terminal = str_detect(rule, '\\)$')
		) %>%
			mutate(
				rule = rule %>% str_remove_all('\\|\\s+') %>%
					str_remove('\\[\\d+\\]') %>%
					str_remove(':.*') %>% str_squish()
			)

		if (eval.ready) {
			rules$rule <- rules$rule %>%
				str_replace(' in ', ' %in% ') %>%
				str_replace_all(c(', ' = '", "', '%\\s+' = '% c("', '(\\D)$' = '\\1")'))
		}

		rules %>% mutate(
			rule = sapply(id, function(this.id) {
				ids <- id[id <= this.id & depth <= depth[id == this.id]]
				#depths <- depth[id <= this.id & depth <= depth[id == this.id]]
				depths <- depth[id %in% ids]

				ids <- tapply(ids, depths, max)

				this.rule <- rule[id %in% ids]
				if (rule.as.text) paste(this.rule, collapse = ' & ') else list(this.rule)
			})
		)
	}
	else if (mod_type == 'rpart') { # just for clarity

		if (nrow(tree$frame) == 1) return(data.frame(rule = character(0), id = numeric(0), depth = numeric(0), n.obs = numeric(0), terminal = logical(0)))

		rules <- tibble(
			rule = out %>% str_squish(),
			id = str_extract(rule, '\\d+\\)') %>% parse_number(),
			depth = setNames(rpart:::tree.depth(1:max(id)), id)[id],
			terminal = str_detect(rule, '\\*')
		) %>%
			mutate(
				rule = rule %>% str_remove('^\\d+\\) ') %>% str_extract('.*?(?= )'),
				n.obs = tree$frame[as.character(id),]$n,
				if (tree$method == 'class') {
					yval <- tree$frame[as.character(id),]$yval2
					if (n_distinct(tree$y) == 2) {
						data.frame(pred = yval[,5])
					} else {
						yval[,(2 + n_distinct(tree$y)):(ncol(yval) - 1)] %>%
							as.data.frame() %>%
							magrittr::set_colnames(paste0('pred', 1:n_distinct(tree$y)))
					}
				} else {
					tree$frame[as.character(id),] %>% transmute(avg = yval, dev)
				}
			)

		if (eval.ready) {
			rules$rule <- rules$rule %>%
				str_replace('=', ' %in% ') %>% {
					ifelse(
						str_detect(., '%in%'),
						ifelse(
							str_detect(.,','),
							str_replace_all(., c(',' = '", "', '%\\s+' = '% c("', 'c\\((.*)$' = 'c(\\1")')),
							str_replace_all(., c('%\\s+' = '% "', "$" = '"'))
						),
						.
					)
				}
		}

		rules %>% mutate(
			rule = sapply(id, function(this.id) {
				ids <- this.id

				while (this.id > 1) {
					this.id <- floor(this.id / 2)
					ids <- c(this.id, ids)
				}

				this.rule <- rule[id %in% ids]

				if (rule.as.text) paste(this.rule, collapse = ' & ') else list(this.rule)
			})
		)
	}


}

#' Simplify ctree rules.
#'
#' Remove redundant components of a rule keeping only the shortest set
#' definition (e.g.: if many conditions in a rule represent nested sets, only
#' those necessary to define the innermost set are kept). The conditions are
#' also rearranged alphabetically for easier comparison.
#'
#' @param rules A character vector of rules joined by the & symbol.
#'
#' @return The same vector of rules after simplification.
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#'
#' @examples
simplify_rules <- function(rules) {
	library(dplyr)
	library(stringr)

	sapply(rules, function(rule) {

		if (rule == '') return(NA)

		components <- str_split(rule, ' & ') %>% unlist
		vars <- str_extract(components, '.* [<>%=in]+') %>% unique
		ind <- sapply(vars, function(v) tail(which(str_detect(components, fixed(v))), 1))

		paste(components[ind] %>% sort, collapse = ' & ')
	}) %>% na.omit
}

clean_filtering_rule <- function(rules) {
	# rules_to_df(rules) %>%
	# 	mutate(
	# 		val = sapply(1:n(), function(i) {
	# 			if (!str_detect(op[i], 'in')) return(val[i])
	# 			values <- str_extract(val[i], 'c\\(.+?\\)')
	# 			rest <- str_remove(val[i], 'c\\(.+?\\) ?')
	#
	# 			values <- eval(parse(text = values)) %>% na.omit()
	#
	# 			if (length(values) > 1) values <- glue("[{paste(values, collapse = ', ')}]")
	#
	# 			paste(values, rest)
	# 		}),
	# 		op = str_replace_all(op, c('<=' = '≤', '>=' = '≥')),
	# 		op = case_when(
	# 			str_detect(val, '\\[') ~ 'in',
	# 			str_detect(op, 'in') ~ '=',
	# 			T ~ op
	# 		)
	# 	) %>%
	# 	group_by(rule) %>%
	# 	summarise(
	# 		new_rule = glue("{var} {op} {val}") %>%
	# 			paste0(collapse = ' & ')
	# 	) %>% pull(new_rule)

	sapply(rules, function(rule) {
		str_split(rule, '&') %>% unlist %>% str_squish %>% sapply(function(x) {

			if (str_detect(x, '%in%')) {
				pieces <- str_split(x, '%in%') %>% unlist %>% str_squish

				values <- eval(parse(text = pieces[2])) %>% na.omit()
				values <- paste0('"', values, '"')

				if (length(values) > 1) values <- glue("in [{paste(values, collapse = ', ')}]")
				else values <- glue("= {values}")

				glue("{pieces[1]} {values}")
			} else str_replace_all(x, c('<=' = '≤', '>=' = '≥'))
		}) %>% sort %>% paste(collapse = ' & ')
	})
}

rules_to_df <- function(rules) {

	if (length(rules) == 0) return(data.frame(rule = character(0), var = character(0), val = character(0)))

	splitted <- rules %>% str_split(' & ')

	lapply(1:length(splitted), function(i) {

		op <- str_extract(splitted[[i]], ' [%in%<=>≤≥]+ ') %>% str_squish()
		struct <- splitted[[i]] %>%
			str_split(' [%in%<=>≤≥]+ ') %>%
			lapply(function(rule) {
				rule <- str_squish(rule)

				data.frame(
					rule = i,
					var = rule[1],
					val = rule[2]
				)
			}) %>% bind_rows()

		struct$op <- op
		struct
	}) %>% bind_rows()
}


add_ecdf_to_rules <- function(rules, data) {

	rule_struct <- rules_to_df(rules)

	vars <- rule_struct %>%
		pull(var) %>% unique

	cumfuncs <- lapply(vars, function(v) {
		if (str_detect(rule_struct$op[rule_struct$var == v], 'in|^=$')) {
			function(x) {

				x <- str_remove_all(x, 'c\\(|[\\)\\[\\]"]') %>%
					paste(collapse = ', ') %>%
					str_split(', ?') %>%
					unlist()

				mean(data[[v]] %in% x)
			}
		} else ecdf(data[[v]])
	}) %>% setNames(vars)

	rule_struct$p = sapply(1:nrow(rule_struct), function(i) {
		rule <- rule_struct[i,]
		if (!is.null(cumfuncs[[rule$var]])) cumfuncs[[rule$var]](rule$val) else NA
	}) %>% percent

	rule_struct %>%
		group_split(rule) %>%
		sapply(function(df) {
			glue_data(df, "{var} {op} {val} ({df$p})") %>%
				paste0(collapse = ' & ')
		})
}


# Modeling -----------------------------------------------------------

create_training_set <- function(Records, pos.mult = 10L) {

	if (pos.mult < 1) stop('pos.mult should be at least 1')

	Records <- Records %>%
		transmute(
			Target = case_when(
				!is.na(Rev_prediction) ~ Rev_prediction,
				!is.na(Rev_abstract) ~ Rev_abstract,
				T ~ Rev_title
			),
			ID, Title, Abstract, Authors, Keywords, Mesh
		) %>% {
			.[c(rep(which(.$Target %in% 'y'), pos.mult), which(!(.$Target %in% 'y'))),]
		}

	message('Title DTM')
	Title_DTM <- with(
		Records,
		text_to_DTM(Title, min.freq = 25, label = 'TITLE__', ids = ID, freq.subset.ids = ID[!is.na(Target)])
	)

	message('dimensions: ', paste(dim(Title_DTM), collapse = ', '))

	message('\nAbstract DTM')
	Abstract_DTM <- with(
		Records,
		Abstract %>%
			str_remove_all(regex('\\b(background|introduction|method\\w*|result\\w*|conclusion\\w*|discussion)',ignore_case = T)) %>%
			text_to_DTM(min.freq = 30, label = 'ABSTR__', ids = ID, freq.subset.ids = ID[!is.na(Target)])
	)

	message('dimensions: ', paste(dim(Abstract_DTM), collapse = ', '))

	message('\nAuthors DTM')
	Authors_DTM <- with(
		Records,
		text_to_DTM(Authors, tokenize.fun = tokenize_authors, min.freq = 20,
								label = 'AUTH__', ids = ID, freq.subset.ids = ID[!is.na(Target)],
								add.ngrams = F, aggr.synonyms = F)
	)

	message('dimensions: ', paste(dim(Authors_DTM), collapse = ', '))

	message('\nKeywords DTM')
	Keywords_DTM <- with(
		Records,
		text_to_DTM(Keywords, min.freq = 30, label = 'KEYS__', ids = ID, freq.subset.ids = ID[!is.na(Target)])
	)

	message('dimensions: ', paste(dim(Keywords_DTM), collapse = ', '))

	message('\nMesh DTM')
	Mesh_DTM <- with(
		Records,
		text_to_DTM(Mesh, tokenize.fun = tokenize_MESH, min.freq = 40,
								label = 'MESH__', ids = ID, freq.subset.ids = ID[!is.na(Target)],
								add.ngrams = F)
	)

	message('dimensions: ', paste(dim(Mesh_DTM), collapse = ', '))

	data.frame(
		Target = Records$Target,
		Title_DTM,
		Abstract_DTM[,-1],
		Authors_DTM[,-1],
		Keywords_DTM[,-1],
		Mesh_DTM[,-1]
	) %>% distinct()

}

summarise_pred_perf <- function(out, quants = c(.5, .05, .95), AUC.thr = .9) {
	summarise(
		out,
		pAUC = percent(mean(AUC >= AUC.thr)),
		across(-pAUC, .fns = ~ {
			val <- quantile(.x, quants) %>% sort %>% percent
			glue("{val[2]} [{val[1]}, {val[3]}]")
		})
	)
}


compute_BART_model <- function(train_data, Y, preds = NULL, save = T,
															 folder = getwd(), name = as.character(Y),
															 rebuild = F, num_trees = 50, k = 2,
															 num_iterations_after_burn_in = 1000, run_in_sample = F,
															 mem_cache_for_speed = T,  use_missing_data = T, verbose = T, ...) {
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

compute_pred_performance <- function(model, data = NULL, Y = NULL, summary = F, quants = c(.05, .5, .95), AUC.thr = .9) {

	library(scales)
	library(pROC)
	library(glue)
	library(pbapply)
	library(pbmcapply)

	if (!is.null(data)) {
		if (is.null(Y)) stop('Define the Y variable name.')

		X <- data %>% select(all_of(model$X %>% colnames()))
		y <- data[[Y]]
	} else {
		X <- model$X
		y <- model$y
	}

	if (is.factor(y)) {
		y <- relevel(y, ref = levels(y)[2])
		y_levels <- levels(y)
	} else {
		y_levels <- sort(unique(y), T)
	}

	samples <- bart_machine_get_posterior(model, new_data = X)$y_hat_posterior_samples

	out <- mclapply(1:ncol(samples), function(i) {
		roc <- pROC::roc(response = y, predictor = samples[,i])
		data.frame(
			AUC = pROC::auc(roc) %>% as.vector(),
			pROC::coords(roc, "best", ret = c('threshold', 'sensitivity', 'specificity', 'accuracy', 'ppv', 'npv'), transpose = F) %>%
				as.list() %>% as.data.frame.list() %>% head(1) %>%
				magrittr::set_colnames(c('Threshold', 'Sens', 'Spec', 'Acc', 'PPV', 'NPV')),
			Sens.thr = quantile(samples[y == y_levels[1], i], min(quants)),
			Spec.thr = quantile(samples[y == y_levels[2], i], max(quants))
		)
	}) %>% bind_rows()

	if (summary) {
		summarise_pred_perf(out, quants)
	} else out

}

enrich_annotation_file <- function(file, DTM = NULL, pos.mult = 10,
																	 n.models = 40, AUC.thr = .9,
																	 perf.quants = c(.01, .5, .99), rebuild = T, ...) {

	perf.quants <- sort(perf.quants)[c(2, 1, 3)]

	if (pos.mult < 1) stop('pos.mult should be at least 1')

	if ((pos.mult - round(pos.mult)) != 0) warning('pos.mult should be an integer; will be rounded up.')

	pos.mult <- round(pos.mult)

	message('Loading file or DTM')

	tictoc::tic()
	Records <- read_excel(file)
	tictoc::toc()

	if (is.null(DTM)) {
		DTM <- create_training_set(Records, pos.mult)
	} else if (is.character(DTM)) {
		DTM <- readr::read_rds(DTM)$DTM
	}


	if (!(all(DTM$ID %in% Records$ID) & all(Records$ID %in% DTM$ID))) stop('The DTM and the records should be compatible (same IDs).')

	message('Model generation')

	if (rebuild == FALSE & file.exists('Model_backup.rds')) {
		message('(loading from disk...)')
		bart.mods <- readr::read_rds('Model_backup.rds')
	}
	else {
		bart.mods <- pblapply(1:n.models, function(i) {
			all_data <- DTM %>% filter(!is.na(Target))

			train_data <- slice_sample(all_data, prop = 1, replace = T) %>% {
				.[c(rep(which(.$Target %in% 'y'), pos.mult), which(!(.$Target %in% 'y'))),]
			}

			test_data <- all_data %>% filter(!(ID %in% train_data$ID))

			bart.mod <- compute_BART_model(train_data %>% select(-ID), 'Target', name = 'BartModel', rebuild = T, save = F, verbose = F, ...)

			out <- list(
				preds = bart_machine_get_posterior(bart.mod, new_data = DTM %>% select(all_of(colnames(bart.mod$X))))$y_hat_posterior_samples,
				oos.perf = compute_pred_performance(bart.mod, data = test_data, Y = 'Target', AUC.thr = AUC.thr, quants = perf.quants),
				var.imp = bartMachine::get_var_props_over_chain(bart.mod, 'trees')
			)
		})

		readr::write_rds(bart.mods, 'Model_backup.rds')
	}

	message('Making predictions')

	tictoc::tic()

	avg_preds <- (Reduce("+", bart.mods %>% lapply(`[[`, 'preds')) / length(bart.mods))

	Predicted_data <- DTM %>% select(ID, Target) %>%
		data.frame(
			mclapply(perf.quants, function(q) {
				apply(avg_preds, 1, quantile, q)
			}) %>% bind_cols() %>% magrittr::set_colnames(c('Pred_Med', 'Pred_Low', 'Pred_Up'))
		) %>%
		mutate(
			Pred_delta = Pred_Up - Pred_Low,
			Predicted_label = case_when(
				Pred_Low > max(Pred_Up[Target %in% 'n']) ~ 'y',
				Pred_Up < min(Pred_Low[Target %in% 'y']) ~ 'n',
				T ~ 'unk'
			),
			Predicted_label = replace(Predicted_label, Predicted_label != Target & Predicted_label != 'unk', 'check'),
			across(matches('Pred_'), signif, 3),
			Target = NULL
		)

	Annotated_data <- left_join(select(Records, -any_of(colnames(Predicted_data %>% select(-ID)))), Predicted_data, by = 'ID') %>%
		select(Order:Rev_text, Predicted_label, Pred_delta, Pred_Med, Pred_Low, Pred_Up, everything()) %>%
		arrange(desc(Pred_delta)) %>%
		mutate(Parent_file = file)

	tictoc::toc()

	message('Adding performance summaries')

	tictoc::tic()

	# oos_perf <- bart.mods %>% lapply('[[', 'oos.perf') %>% bind_rows() %>% # without averaging
	oos_perf <- (Reduce("+", bart.mods %>% lapply(function(x) x$oos.perf)) / length(bart.mods)) %>%
		summarise_pred_perf(quants = perf.quants, AUC.thr = AUC.thr) %>% t

	ins_perf <- local({

		samples <- avg_preds[!is.na(DTM$Target),]
		DTM <- filter(DTM, !is.na(Target))

		y <- DTM$Target
		y_levels <- if (is.factor(y)) levels(y) else sort(unique(y))

		mclapply(1:ncol(avg_preds), function(i) {
			roc <- pROC::roc(response = y, predictor = samples[,i])

			data.frame(
				AUC = pROC::auc(roc) %>% as.vector(),
				pROC::coords(roc, "best", ret = c('threshold', 'sensitivity', 'specificity', 'accuracy', 'ppv', 'npv'), transpose = F) %>%
					as.list() %>% as.data.frame.list() %>% head(1) %>%
					magrittr::set_colnames(c('Threshold', 'Sens', 'Spec', 'Acc', 'PPV', 'NPV')),
				Sens.thr = quantile(samples[y == y_levels[2], i], min(perf.quants)),
				Spec.thr = quantile(samples[y == y_levels[1], i], max(perf.quants))
			)
		}) %>% bind_rows() %>%
			summarise_pred_perf(quants = perf.quants, AUC.thr = AUC.thr) %>%
			mutate(
				Cases = {
					tbl <- table(Predicted_data$Predicted_label)
					sprintf(
						'%s: %d (%s)',
						names(tbl),
						tbl,
						percent(as.vector(prop.table(tbl)))
					) %>% paste(collapse = '; ')
				},
				UnkToLabel = with(Annotated_data, sum(Predicted_label == 'unk' & is.na(Rev_prediction))),
				PosToLabel = with(Annotated_data, sum(Predicted_label == 'y' & is.na(Rev_prediction) & is.na(Rev_abstract))),
			) %>% t
	})

	tictoc::toc()

	message('Adding variables\' importance')

	var_imp <- lapply(1:length(bart.mods), function(i) {
		data.frame(
			Term = names(bart.mods[[i]]$var.imp),
			Val = bart.mods[[i]]$var.imp
		)
	}) %>% bind_rows() %>%
		group_by(Term) %>%
		summarise(Value = mean(Val), Score = Value / sd(Val), n.models = n())

	out <- list(
		Annotated_data = Annotated_data,
		In_sample_perf = data.frame(Indicator = rownames(ins_perf), Value = ins_perf),
		Out_of_sample_perf = data.frame(Indicator = rownames(oos_perf), Value = oos_perf),
		Variable_importance = var_imp
	)

	message('Exporting')

	tictoc::tic()

	time_stamp <- safe_now()

	WriteXLS::WriteXLS(out, ExcelFileName = file.path('Annotations', paste0('Records_P_', time_stamp, '.xlsx')))
	tictoc::toc()

	tictoc::tic()
	out$Prediction_matrix <- avg_preds
	out$DTM <- DTM

	readr::write_rds(out, file.path('Models', paste0('Results_', time_stamp, '.rds')), compress = 'gz', )
	tictoc::toc()

	invisible(out)
}

select_best_rules <- function(trees, stat.filter = NULL, only.terminal = F, only.inclusive.rules = F, target.vec, target.data, algorithm = c('linear', 'sequential')) {
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
	}) %>% bind_cols() %>% magrittr::set_colnames(paste0('V__', specific.terms)) %>%
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

summarise_annotations <- function(annotation.folder = 'Annotations', plot = T) {
	# list.files('Models') %>% pbmclapply(function(file) {
	# 	Model <- read_rds(file.path('Models', file))

	Res <- list.files(annotation.folder, recursive = T, pattern = 'Records_P') %>%
		str_subset('~\\$', negate = T) %>%
		pbmclapply(function(file) {

			Annotated_data <- read_excel(file.path('Annotations', file))

			Performance <- read_excel(file.path('Annotations', file), sheet = 'Out_of_sample_perf')
			Performance <- as.list(Performance$Value) %>% setNames(Performance$Indicator)

			Var_imp <- read_excel(file.path('Annotations', file), sheet = 'Variable_importance') %>%
				filter(!str_detect(Term, 'MESH|KEYS')) %>%
				mutate(Term = str_remove(Term, '.+__')) %>%
				group_by(Term) %>% slice_max(order_by = Score) %>% ungroup() %>%
				arrange(desc(Score)) %>% pull(Term) %>% head(15) %>%
				str_replace('^\\w', str_to_upper) %>%
				str_replace_all(c('\\._\\.' = ' ', '\\.' = ' OR ', '_' = ' ')) %>%
				paste(collapse =', ')

			Predicted <- Annotated_data$Predicted_label
			Manual <- with(Annotated_data, case_when(
				!is.na(Rev_prediction) ~ as.character(Rev_prediction),
				!is.na(Rev_abstract) ~ Rev_abstract,
				T ~ Rev_title
			))

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
