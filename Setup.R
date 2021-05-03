
# Setup -------------------------------------------------------------------
Sys.setenv(LANG = "en")
if (is.null(options('BartMem')[[1]])) {
	mem <- readline("How much GB of memory should be used (better no more than 90% of available one)?")

	if (is.na(as.numeric(mem))) stop('input should be a number.')

	mem <- paste0("-Xmx", mem, "g")

	options(BartMem = mem)
}

options(java.parameters = options('BartMem')$BartMem)

local({
	install_and_load <- c("dplyr", "stringr", "glue", "readr", "readxl", "lubridate", "Matrix", "igraph", "pbapply",
												"pbmcapply", "rpart", "bartMachine", "tm", "patchwork", "ggplot2", "ggrepel", "RLesur/crrri",
												"bakaburg1/tidytrees")
	only_install <- c('purrr', 'openxlsx', 'tictoc', 'tidyr', 'arm',
										'parallel', 'jsonlite', 'rentrez', 'wosr', 'brms')

	if (!('devtools' %in% installed.packages())) install.packages('devtools')

	for (pkg in setdiff(c(install_and_load, only_install), installed.packages())) {
		if (grepl('/', pkg)) try(devtools::install_github(pkg)) else install.packages(pkg)
	}

	for (pkg in install_and_load) {
		try(library(stringr::str_remove(pkg, '^.*/'), character.only = TRUE))
	}
})


### Windows do not support mclapply
if  (.Platform$OS.type != 'unix') {
	mclapply <- lapply
	pbmclapply <- pblapply
}

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

# override lubridate equivalents which always complain about the missing timezone
today <- function() as_date(Sys.time())
now <- function() Sys.time()

# A file path friendly lubridate::now()
safe_now <- function() {
	str_replace_all(now(), c(' ' = 'T', ':' = '.'))
}

summarise_vector <- function(vec) {
	if (length(vec) == 0) return(0)
	table(vec) %>%
		{paste0(names(.), ': ', ., ' (', percent(./sum(.)), ')', collapse = ', ')}
}

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

# Parse an excel/csv file or return data if already parsed
import_data <- function(input) {

	if (is.character(input) | is.factor(input)) {
		if (str_detect(input, '\\.xlsx?$')) {
			return(read_excel(input, guess_max = 10^6))
		} else if (str_detect(input, '\\.xlsx?$')) {
			return(read_csv(input, guess_max = 10^6))
		}
	} else if (is.data.frame(input)) {
		return(input)
	}

	stop('Input should be an existing csv/excel file path or a data.frame')
}

summarise_args <- function(args) {
	print(args %>% names())
	args %>%
		names %>% lapply(function(name) {

			if (name == '' || !exists(name)) return(NULL)
			obj <- get(name)
			data.frame(
				name,
				value = if (is.data.frame(obj)) paste(class(obj), collapse = ', ') else capture.output(str(obj)) %>% head() %>% paste(collapse = '\n') %>% str_trim()
			)
		}) %>% bind_rows()
}



# Record search -----------------------------------------------

clean_date_filter_arg <- function(year_query, cases,
																	arg_in_query_test = NULL, query = NULL) {

	if (class(year_query) %nin% c('NULL', 'character') | length(year_query) > 1) {
		stop('Year filter query should be a single character string or NULL')
	}

	if (is.character(year_query)) {

		year_query <- str_remove_all(year_query, '\\s+|^\\(|\\)$')

		if (!is.null(arg_in_query_test) & !is.null(query)) {
			if (str_detect(query, fixed(arg_in_query_test))) {
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
		transmute(Order = 1:n(), ID = ut, Title = title, Abstract = abstract,
							DOI = doi, Journal = journal, N_citations = tot_cites,
							Published = format(ymd(date),'%b %Y'), Source = 'WOS',
							Source_type = 'API', Creation_date = safe_now())

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

	records <- records %>% clean_record_textfields()

	message('...found ', nrow(records), ' records.')

	records
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
		range = '{year_piece[1]}[PDAT]:{year_piece[2]}[PDAT]',
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
		have.results <- F
		trials <- 0

		while (!have.results & trials < 20) { # not efficient but the other solution (below) fails for some reason
			rescords <- try(rentrez::entrez_fetch(db = "pubmed", web_history = res$web_history,
																						retstart = step * 200, retmax = min(200, total_count - step * 200),
																						rettype = 'medline', parsed = F,
																						api_key = options('ncbi_api_key')), silent = T)

			have.results <- class(rescords) == 'character'
			trials <- trials + 1
		}

		rescords
	})

	if (sapply(records, function(x) class(x) == 'try-error') %>% any) {
		stop('Couldn\'t get results for some steps after 20 attempts')
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
				ID = paste0('IEEE:', str_extract(abstract_url, '\\d+/$')),
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

	records <- left_join(records, article_data, by = 'URL') %>%
		mutate(Source = 'IEEE', Source_type = 'API', Creation_date = now()) %>%
		clean_record_textfields() %>%
		select(Order, ID, Title, Abstract, DOI, URL, Authors, Journal,
					 Article_type, Author_keywords, Keywords, Mesh, N_citations,
					 Published, Source, Source_type)

	message('...found ', nrow(records), ' records.')

	records
}

perform_search_session <- function(query, year_query = NULL, actions = c('API', 'parsed'),
																	 sources = c('IEEE', 'WOS', 'Pubmed', 'Scopus', 'Embase'),
																	 session_name = 'Session1', query_name = 'Query1',
																	 records_folder = 'Records', overwrite = FALSE,
																	 journal = 'Session_journal.csv') {

	load_if_exists <- function(out_file, overwrite) {

		if (file.exists(out_file)) {
			if (!overwrite) {
				warning(basename(out_file), ' already present and argument overwrite == FALSE.', call. = F)

				read_csv(out_file, col_types = cols())
			} else {
				warning(basename(out_file), ' will be overwritten.', call. = F)
				NULL
			}
		} else NULL

	}

	folder_path <- file.path(records_folder, session_name, query_name)

	if (!dir.exists(folder_path)) dir.create(folder_path, recursive = T)

	search_ts <- now()

	input_files <- NA

	record_data <- lapply(sources, function(source) {

		lapply(actions, function(action) {

			records <- NULL

			if (action == 'API') {
				## API search

				output_file <- file.path(folder_path, glue('{source}_API.csv'))

				records <- load_if_exists(output_file, overwrite)  # load records if output already existing

				search_fun <- paste0('search_', str_to_lower(source))

				if (is.null(records) & exists(search_fun)) { # if output not existing search via API and API search is available

					records <- get(search_fun)(query = query, year_query = year_query)
				}

			} else if (action == 'parsed') {
				## Parsing downloaded records

				# find input files (i.e. files not containing API or parsed in the name)
				input_files <- list.files(folder_path, full.names = F) %>%
					str_subset('API|parsed', negate = T) %>%
					str_subset(regex(source, ignore_case = T))

				if (length(input_files) > 0) { # continue if any input file exists

					output_file <- file.path(folder_path, glue('{source}_parsed.csv'))

					records <- load_if_exists(output_file, overwrite) # load records if output already existing

					if (is.null(records)) { # in output not existing parse the raw data
						records <- file.path(folder_path, input_files) %>%
							read_bib_files() %>%
							bind_rows()
					}

					input_files <- paste(input_files, collapse = ', ')
				} else input_files <- NA
			}

			if (!is.null(records)) {

				records$FileID <- file.path(session_name, query_name, basename(output_file))

				write_csv(records, output_file)

				data.frame(
					Session_ID = session_name,
					Query_ID = query_name,
					Source = source,
					Type = action,
					Input_files = input_files,
					Output_file = basename(output_file),
					Timestamp = search_ts,
					Filter = year_query,
					N_results = nrow(records),
					Query = query
				)
			}

		})

	}) %>% bind_rows()

	if (nrow(record_data) == 0) {
		warning('No records were added', call. = F)
	}

	if (!is.null(journal)) {

		if (str_detect(journal, '\\.csv$')) {
			write_fun <- write_csv
			read_fun <- function(x) read_csv(x, col_types = cols())
		} else if (str_detect(journal, '\\.xlsx?$')) {
			write_fun <- function(x, file) openxlsx::write.xlsx(x, file, asTable = T)
			read_fun <- read_excel
		} else stop('Session journal file type must be csv or excel.')

		if (file.exists(journal)) {
			previous_data <- read_fun(journal)

			record_data <- previous_data %>%
				bind_rows(record_data) %>%
				group_by(Session_ID, Query_ID, Source, Type) %>%
				arrange(Timestamp, .by_group = T) %>%
				summarise(across(.fns = last))
		}

		write_fun(record_data, journal)
	}

	record_data

}


# Record data management -------------------------------------------------

clean_record_textfields <- function(df) {
	mutate(df,
				 across(where(is.character),
				 			 ~ str_replace_all(.x, c(' *; *' = ';', '["\']+' = ' ')) %>%
				 			 	str_squish() %>%
				 			 	{replace(., . %in% c('', 'NA'), NA)}
				 )
	)
}

extract_source_file_paths <- function(journal, sessions = journal$Session_ID,
																			queries = journal$Query_ID,
																			sources = journal$Source,
																			records_folder = 'Records') {
	journal %>% filter(Session_ID %in% sessions, Query_ID %in%  queries,
										 Source %in% sources) %>%
		with(file.path(records_folder, Session_ID, Query_ID, Output_file)) %>%
		unique()
}

parse_medline <- function(entries, timestamp = now()) {
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

	info$PMID = str_extract(entries, '(?<=PMID- )\\d+')

	info %>% transmute(
		Order = 1:n(),
		ID = paste0('PMID:', PMID), Title = coalesce(TI, BTI),
		Abstract = AB, DOI = coalesce(LID, AID),
		Authors = FAU, URL = paste0('https://pubmed.ncbi.nlm.nih.gov/', PMID),
		Journal = JT, Journal_short = TA, Article_type = PT, Mesh = MH,
		Author_keywords = OT, Published = DP,
		Source = 'Pubmed',
		Creation_date = timestamp
	) %>% clean_record_textfields()
}

parse_wos <- function(entries, timestamp = now()) {
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
		Creation_date = timestamp
	) %>% clean_record_textfields()
}

parse_ieee <- function(entries, timestamp = now()) {
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
		Creation_date = now()
	) %>% clean_record_textfields()
}

parse_embase <- function(entries, timestamp = now()) {
	entries %>% transmute(
		Order = 1:n(),
		ID = paste0('EM:', PUI),
		Title,
		Abstract, DOI, URL = paste0('https://www.embase.com/a/#/search/results?id=', PUI),
		Authors = `Author Names` %>%
			str_replace_all(c('\\s*,\\s*' = ';', ' (?=\\w\\.)' = ', ', '\\.' = ' ')),
		Journal = `Source title`,
		Author_keywords = `Author Keywords` %>% str_replace_all('\\s*,\\s*', ';'),
		Keywords = select(cur_data(), contains('Emtree')) %>%
			apply(1, function(x) na.omit(x) %>% paste(collapse = ', ')) %>%
			str_replace_all(c(
				'\\s*,\\s*' = ';',
				'\\(.+\\)' = ''
			)),
		Article_type = `Publication Type`,
		Published = `Date of Publication`,
		Source = 'Embase',
		Source_type = 'parsed',
		Creation_date = timestamp
	) %>% clean_record_textfields()
}

parse_scopus <- function(entries, timestamp = now()) {
	entries %>% transmute(
		Order = 1:n(),
		ID = paste0('SCP:', EID),
		Title,
		Abstract = str_remove(Abstract, fixed('[No abstract available]')),
		DOI, URL = Link,
		Authors = Authors %>%
			str_replace_all(c('\\s*,\\s*' = ';', ' (?=\\w\\.)' = ', ', '\\.' = ' ')),
		Journal = `Source title`,
		Author_keywords = `Author Keywords`,
		Keywords = `Index Keywords`,
		Article_type = `Document Type`,
		N_citations = `Cited by`,
		Published = Year,
		Source = 'Scopus',
		Source_type = 'parsed',
		Creation_date = timestamp
	) %>% clean_record_textfields()
}

read_bib_files <- function(files) {

	ts <- now()

	pblapply(files, function(file) {

		if (str_detect(file, '(parsed|API)\\.csv')) {  # no parsing necessary
			message('Reading ', basename(file), '...')

			return(read_csv(file, col_types = cols()))
		}

		message('Parsing ', basename(file), '...')

		type <- NULL

		if (str_detect(file, '\\.(nbib|txt)$')) {
			entries <- read_file(file)

			if (str_detect(entries, 'PMID-')) type <- 'medline'

		} else if (str_detect(file, '\\.(xlsx?|csv)$')) {
			entries <- if (str_detect(file, '\\.csv$')) read_csv(file, col_types = cols()) else read_excel(file)

			if ('UT (Unique WOS ID)' %in% colnames(entries)) type <- 'wos'
			else if ('IEEE Terms' %in% colnames(entries)) type <- 'ieee'
			else if ('Scopus' %in% entries$Source) type <- 'scopus'
			else if ('Embase Accession ID' %in% colnames(entries)) type <- 'embase'
		}

		if (is.null(type)) {
			warning('Format not recognized for ', file)
			return(NULL)
		}

		get(paste0('parse_', type))(entries, ts) %>%
			data.frame()
	}) %>% setNames(basename(files))
}

join_records <- function(record.list) {
	lapply(record.list, function(source) {
		source %>%
			transmute(
				Order,
				DOI, ID, Title, Abstract, Authors, Year = Published %>% str_extract('\\d{4}') %>% as.numeric(),
				URL = if (exists('URL')) URL else NA,
				Journal = if (exists('Journal')) Journal else NA,
				Journal_short = if (exists('Journal_short')) Journal_short else NA,
				Keywords = if (exists('Keywords')) Keywords else NA,
				Author_keywords = if (exists('Author_keywords')) Author_keywords else NA,
				Mesh = if (exists('Mesh')) Mesh else NA,
				Article_type,
				N_citations = if (exists('N_citations')) N_citations else NA,
				Source, Source_type,
				FileID = if (exists('FileID')) FileID else NA,
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
		distinct() %>%
		arrange(Order)
}

order_by_query_match <- function(records, query) {
	terms <- str_remove_all(query, '[^\\w\\s\\*]+|(?<= )(AND|OR)(?= )') %>%
		str_split('\\s+') %>%
		unlist() %>% unique() %>%
		str_replace_all('\\*', '\\\\w*')

	records %>%
		mutate(
			text = paste(Title, Abstract),
			doc.length = str_count(text, '\\b') + 1,
			term.count = str_count(text, paste(terms, collapse = '|')),
			score = term.count/doc.length
		) %>%
		arrange(desc(score)) %>%
		mutate(Order = 1:n()) %>%
		select(-text, -doc.length, -term.count, -score)
}

create_annotation_file <- function(records, reorder_query = NULL,
																 prev_records = NULL,
																 prev_classification = NULL,
																 out_type = c('xlsx', 'csv')) {

	out_type <- match.arg(out_type)

	if (class(records) %nin% c('character', 'list', 'data.frame')) {
		stop('"records" should be either of vector of file/folder paths, a list of data.frame or a single data.frame')
	}

	if (is.character(records)) {
		records <- c(
			list.files(records, full.names = T, recursive = T) %>%
				str_subset('~\\$', negate = T),
			records[!dir.exists(records)]
		)

		message('- parsing records...')
		records <- read_bib_files(records)
	}

	if (length(records) == 1) records <- records[[1]]

	if (!is.data.frame(records) & is.list(records)) {
		message('- joining records...')
		records <- join_records(records)
		message(": ", nrow(records), ' unique records')
	}

	records <- records %>%
		mutate(
			Rev_manual = NA,
			.before = DOI
		)

	if (!is.null(prev_records)) {
		message('- appending to a previous annotation file...')

			imported_records <- import_data(prev_records)

			records <- records %>% filter(!(ID %in% imported_records$ID))

			message("(", nrow(records), ' new records)')

			records <- full_join(
				import_data(prev_records), records
			) %>%
				fix_duplicated_records()
	}

	if (!is.null(prev_classification)) {
		message('- importing previous classifications...')

		imported_records <- import_data(prev_classification)

		records <- import_classification(records, prev_records = prev_records)
	}

	if (!is.null(reorder_query)) {
		message('- reordering records...')

		records <- order_by_query_match(records, query = reorder_query)
	}

	# message('- saving records...')
	#
	# folder <- file.path(sessions_folder, session_name)
	# if (!dir.exists(folder)) dir.create(folder, recursive = T)
	#
	# file <- file.path(folder, paste0('Records_', safe_now(), '.', out_type))
	#
	# if (out_type == 'xlsx') {
	# 	openxlsx::write.xlsx(records, file = file, asTable = T)
	# } else write_csv(records, file = file)

	invisible(records)
}

create_session <- function(Records, session_name,
													 sessions_folder = 'Sessions', DTM = NULL,
													 dup_session_action = c('skip', 'stop', 'add', 'replace'),
													 use_time_stamp = TRUE) {

	dup_session_action <- match.arg(dup_session_action)

	initialise_session <- function(Records, session_path, DTM = NULL,
																 use_time_stamp = T) {

		if (use_time_stamp) ts <- glue('_{safe_now()}') else ''

		# Create the session folder
		dir.create(session_path, recursive = T, showWarnings = FALSE)

		# At the moment csv files will be converted to excel, eventually both file
		# type will be supported
		if (is.character(Records) && str_detect(Records, '\\.csv$')) Records <- import_data(Records)

		# Copy or write the Record data

		file_path <- file.path(session_path, glue('Records{ts}.xlsx'))
		if (is.character(Records) | is.factor(Records)) {
			if (!file.exists(Records)) stop(Records, ' does not exists.')

			file.copy(Records, file_path, overwrite = T, recursive = F)
		} else {
			openxlsx::write.xlsx(Records, file = file_path, asTable = T)
		}

		# Copy or write the DTM data
		file_path <- file.path(session_path, 'DTM.rds')
		if (!is.null(DTM)) {
			if (is.character(DTM) | is.factor(DTM)) {
				if (!file.exists(DTM)) stop(DTM, ' does not exists.')

				file.copy(DTM, file_path, overwrite = T, recursive = F)
			} else {
				readr::write_rds(DTM, file = file_path, asTable = T)
			}
		}
	}

	session_path <- file.path(sessions_folder, session_name)

	if (dir.exists(session_path)) {
		switch(dup_session_action,
					 skip = {
					 	warning('Session "', session_name, '" exists. Skipping...')
					 	return(session_path)
					 	},
					 add = {
					 	warning('Session "', session_name, '" exists. Adding a replicate...')
					 	cur_rep <- max(str_extract(session_name, '(?<=_r)\\d+') %>% as.numeric(), 1, na.rm = T)

					 	session_name <- str_remove(session_name, '_r\\d+$') %>% paste0('_r', cur_rep + 1)

					 	session_path <- create_session(Records = Records, session_name = session_name,
					 																 sessions_folder = sessions_folder, DTM = DTM,
					 																 dup_session_action = dup_session_action)
					 },
					 replace = {
					 	warning('Session "', session_name, '" exists. Replacing...')
					 	failure <- unlink(session_path, recursive = T)

					 	if (failure == 1) stop('Session removal failed!')
					 },
					 stop = stop('Session "', session_name, '" is already existing. Stopping...')
		)
	} else {
		message('Create session folder "', session_path, '".')
	}

	initialise_session(Records, session_path, DTM, use_time_stamp)

	return(session_path)
}

fix_duplicated_records <- function(records) {

	records <- records %>%
		group_by(ID) %>%
		mutate(Title = na.omit(Title)[1]) %>%
		ungroup() %>%
		mutate(
			UID = str_to_lower(Title) %>% str_remove_all('[^\\w\\d]+')
		) %>%
		group_by(UID) %>%
		mutate(DOI = na.omit(DOI)[1]) %>%
		ungroup() %>%
		mutate(
			UID = coalesce(DOI, UID)
		)

	dup_recs <- records$UID[duplicated(records$UID)]

	unique_sources <- records %>% filter(!(UID %in% dup_recs))
	dup_sources <- records %>% filter(UID %in% dup_recs)

	dup_sources <- dup_sources %>%
		group_by(UID) %>%
		summarise(
			Order = min(Order),
			across(any_of(c('Title', 'Abstract', 'Authors', 'Journal', 'Journal_short',
											'Year', "Pred_delta", "Pred_Med", "Pred_Low", "Pred_Up")),
						 ~ na.omit(.x)[1]),
			across(any_of(c('ID', 'DOI', 'URL', 'Mesh', 'Article_type', 'Source',
											'Source_type', 'FileID', "Rev_manual", "Rev_prediction",
											"Rev_previous", "Predicted_label")),
						 ~ na.omit(.x) %>% unique() %>% paste(collapse = '; ')),
			Keywords = Keywords %>% str_split('; ') %>% unlist() %>% na.omit() %>% unique() %>%
				purrr::keep(~ str_length(.x) > 0) %>%
				paste(collapse = '; '),
			N_citations = suppressWarnings(na.omit(N_citations) %>% max(na.rm = T) %>% purrr::modify_if(~ !is.finite(.x), ~ NA))
		)

	bind_rows(unique_sources, dup_sources) %>% select(-UID) %>%
		clean_record_textfields() %>%
		filter(!duplicated(ID))
}

summarise_by_source <- function(annotation_file) {
	data <- if (is.character(annotation_file)) {
		read_excel(annotation_file)
	} else annotation_file

	sources <- data$Source %>% str_split(., '; ') %>% unlist() %>% table()

	c(setNames(as.vector(sources), names(sources)), Total = nrow(data))
}

import_classification <- function(records, prev_records, IDs = records$ID) {

	prev_records <- import_data(prev_records)

	records$uID = with(records,
										 ifelse(!is.na(DOI), DOI, str_to_lower(Title) %>%
										 			 	str_remove_all('[^\\w\\d\\s]+')))

	prev_records$uID = with(prev_records,
													ifelse(!is.na(DOI), DOI, str_to_lower(Title) %>%
																 	str_remove_all('[^\\w\\d\\s]+')))

	target_uID <- records$uID[records$ID %in% IDs]
	prev_records <- filter(prev_records, uID %in% target_uID)

	# if ('Rev_title' %in% colnames(prev_records)) {
	# 	prev_records <- prev_records %>%
	# 		transmute(
	# 			Rev_manual = coalesce_labels(., c('Rev_abstract', 'Rev_title')),
	# 			Rev_prediction = Rev_prediction,
	# 			uID
	# 		)
	# }

	prev_records <- prev_records %>% transmute(
		uID,
		Rev_previous = coalesce_labels(cur_data(), c('Rev_previous',
																								 'Rev_prediction_new',
																								 'Rev_prediction', 'Rev_manual',
																								 'Rev_abstract', 'Rev_title'))
	) %>% distinct()

	left_join(records, prev_records, by = 'uID') %>% {
		if ('Rev_previous.y' %in% colnames(.)) {
			mutate(.,
						 Rev_previous = coalesce(Rev_previous.y, Rev_previous.x),
						 .after = any_of(c('Rev_prediction_new', 'Rev_prediction', 'Rev_manual'))
			) %>% select(-Rev_previous.y, -Rev_previous.x)
		} else .
	} %>%
		select(Order, contains('Rev_'), Rev_previous, everything()) %>%
		select(-uID)
}

check_classification_trend <- function(records, column = NULL,
																			 step_size = 20, limit = NULL) {

	if (is.null(column)) {
		records <- records %>%
			mutate(Target = coalesce_labels(., c('Rev_prediction', 'Rev_manual')))
	} else records$Target <- records[[column]]

	records <- records %>% arrange(Order) %>%
		filter(!is.na(Target))

	if (is.null(limit)) limit <- max(which(!is.na(records$Target)))
	steps <- seq(step_size, limit, by = step_size) %>% c(limit) %>% unique()

	df <- pblapply(steps, function(step) {
		records %>% head(step) %>%
			summarise(
				Yes = sum(Target == 'y', na.rm = T),
				No = sum(Target == 'n', na.rm = T)
			)
	}) %>% bind_rows()

	p <- df %>%
		ggplot(aes(x = steps)) +
		geom_line(aes(y = Yes, color = 'yes'), size = 1) +
		geom_line(aes(y = No, color = 'no'), size = 1) +
		labs(y = 'Records', x = 'Batch size', color = 'Classification') +
		theme_minimal()

	df <- mutate(
		df,
		across(c(Yes, No), ~ c(.x[1], sapply(2:(n() - 1), function(i) {
			if (.x[i] == .x[i-1]) NA else .x[i]
		}), .x[n()]))
	)

	p +
		geom_label(aes(y = Yes, x = steps, label = Yes), data = df, alpha = .8) +
		geom_label(aes(y = No, x = steps, label = No), alpha = .8)
}



# NLP ---------------------------------------------------------------------

lemmatize <- function(text.vec, dict = lexicon::hash_lemmas,
											separator = '_tagseparator_') {

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
	}) %>% unlist %>%
		str_replace_all('; *', ' ')

	tictoc::toc()

	output
}

tokenize_keywords <- function(keywords) {
	keywords %>%
		str_to_lower() %>%
		str_replace_all(c(
			'\\s*;\\s*' = ';',
			'[^;\\w]+' = '_',
			';' = ' '))
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


text_to_DTM <- function(corpus, min.freq = 20, ids = 1:length(corpus),
												freq.subset.ids = ids,
												included.pos = c('Noun', 'Verb', 'Adjective'),
												tokenize.fun = tokenize_text, add.ngrams = T,
												aggr.synonyms = T, n.gram.thresh = .5,
												syn.thresh = .9, label = 'TERM__', na.as.missing = T) {

	raw.corpus <- corpus
	order.ids <- 1:length(corpus)
	names(ids) <- order.ids

	if (is.na(min.freq)) stop('"min.freq" is NA.')

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
	) %>% na.omit() %>% distinct() %>%
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

	# The synonyms creation procedure can create very long names
	DTM <- DTM %>% setNames(str_sub(colnames(DTM), 1, 10000))

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
	} else DTM
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
							setNames(paste0('pred', 1:n_distinct(tree$y)))
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

#' #' Simplify ctree rules.
#' #'
#' #' Remove redundant components of a rule keeping only the shortest set
#' #' definition (e.g.: if many conditions in a rule represent nested sets, only
#' #' those necessary to define the innermost set are kept). The conditions are
#' #' also rearranged alphabetically for easier comparison.
#' #'
#' #' @param rules A character vector of rules joined by the & symbol.
#' #'
#' #' @return The same vector of rules after simplification.
#' #'
#' #' @import dplyr
#' #' @import stringr
#' #'
#' #' @export
#' #'
#' #' @examples
#' simplify_rules <- function(rules) {
#' 	library(dplyr)
#' 	library(stringr)
#'
#' 	sapply(rules, function(rule) {
#'
#' 		if (rule == '') return(NA)
#'
#' 		components <- str_split(rule, ' & ') %>% unlist
#' 		vars <- str_extract(components, '.* [<>%=in]+') %>% unique
#' 		ind <- sapply(vars, function(v) tail(which(str_detect(components, fixed(v))), 1))
#'
#' 		paste(components[ind] %>% sort, collapse = ' & ')
#' 	}) %>% na.omit
#' }
#'
#' clean_filtering_rule <- function(rules) {
#' 	# rules_to_df(rules) %>%
#' 	# 	mutate(
#' 	# 		val = sapply(1:n(), function(i) {
#' 	# 			if (!str_detect(op[i], 'in')) return(val[i])
#' 	# 			values <- str_extract(val[i], 'c\\(.+?\\)')
#' 	# 			rest <- str_remove(val[i], 'c\\(.+?\\) ?')
#' 	#
#' 	# 			values <- eval(parse(text = values)) %>% na.omit()
#' 	#
#' 	# 			if (length(values) > 1) values <- glue("[{paste(values, collapse = ', ')}]")
#' 	#
#' 	# 			paste(values, rest)
#' 	# 		}),
#' 	# 		op = str_replace_all(op, c('<=' = '≤', '>=' = '≥')),
#' 	# 		op = case_when(
#' 	# 			str_detect(val, '\\[') ~ 'in',
#' 	# 			str_detect(op, 'in') ~ '=',
#' 	# 			T ~ op
#' 	# 		)
#' 	# 	) %>%
#' 	# 	group_by(rule) %>%
#' 	# 	summarise(
#' 	# 		new_rule = glue("{var} {op} {val}") %>%
#' 	# 			paste0(collapse = ' & ')
#' 	# 	) %>% pull(new_rule)
#'
#' 	sapply(rules, function(rule) {
#' 		str_split(rule, '&') %>% unlist %>% str_squish %>% sapply(function(x) {
#'
#' 			if (str_detect(x, '%in%')) {
#' 				pieces <- str_split(x, '%in%') %>% unlist %>% str_squish
#'
#' 				values <- eval(parse(text = pieces[2])) %>% na.omit()
#' 				values <- paste0('"', values, '"')
#'
#' 				if (length(values) > 1) values <- glue("in [{paste(values, collapse = ', ')}]")
#' 				else values <- glue("= {values}")
#'
#' 				glue("{pieces[1]} {values}")
#' 			} else str_replace_all(x, c('<=' = '≤', '>=' = '≥'))
#' 		}) %>% sort %>% paste(collapse = ' & ')
#' 	})
#' }
#'
#' rules_to_df <- function(rules) {
#'
#' 	if (length(rules) == 0) return(data.frame(rule = character(0), var = character(0), val = character(0)))
#'
#' 	splitted <- rules %>% str_split(' & ')
#'
#' 	lapply(1:length(splitted), function(i) {
#'
#' 		op <- str_extract(splitted[[i]], ' [%in%<=>≤≥]+ ') %>% str_squish()
#' 		struct <- splitted[[i]] %>%
#' 			str_split(' [%in%<=>≤≥]+ ') %>%
#' 			lapply(function(rule) {
#' 				rule <- str_squish(rule)
#'
#' 				data.frame(
#' 					rule = i,
#' 					var = rule[1],
#' 					val = rule[2]
#' 				)
#' 			}) %>% bind_rows()
#'
#' 		struct$op <- op
#' 		struct
#' 	}) %>% bind_rows()
#' }
#'
#'
#' add_ecdf_to_rules <- function(rules, data) {
#'
#' 	rule_struct <- rules_to_df(rules)
#'
#' 	vars <- rule_struct %>%
#' 		pull(var) %>% unique
#'
#' 	cumfuncs <- lapply(vars, function(v) {
#' 		if (str_detect(rule_struct$op[rule_struct$var == v], 'in|^=$')) {
#' 			function(x) {
#'
#' 				x <- str_remove_all(x, 'c\\(|[\\)\\[\\]"]') %>%
#' 					paste(collapse = ', ') %>%
#' 					str_split(', ?') %>%
#' 					unlist()
#'
#' 				mean(data[[v]] %in% x)
#' 			}
#' 		} else ecdf(data[[v]])
#' 	}) %>% setNames(vars)
#'
#' 	rule_struct$p = sapply(1:nrow(rule_struct), function(i) {
#' 		rule <- rule_struct[i,]
#' 		if (!is.null(cumfuncs[[rule$var]])) cumfuncs[[rule$var]](rule$val) else NA
#' 	}) %>% percent
#'
#' 	rule_struct %>%
#' 		group_split(rule) %>%
#' 		sapply(function(df) {
#' 			glue_data(df, "{var} {op} {val} ({df$p})") %>%
#' 				paste0(collapse = ' & ')
#' 		})
#' }


# Modeling -----------------------------------------------------------

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

	min_freq <- max(floor(sum(Records$Target %in% c('y', 'n')) * min_freq), 1)

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
# 																		 quants = c(.05, .5, .95), AUC.thr = .9) {
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

# compute_pred_performance2 <- function(DTM, models, perf_quants = c(.01, .5, .99),
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
# 				ID_preds %>% quantile(perf_quants) %>% t %>%
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
# 			}) %>% unlist() %>% quantile(perf_quants) %>% percent() %>% {glue("{.[2]} [{.[1]}, {.[3]}]")},
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

compute_pred_performance <- function(data, samples = NULL, test_data = NULL,
																		 negLim = NULL, posLim = NULL,
																		 truePos = NULL, trueNeg = NULL,
																		 perf_quants = c(.01, .5, .99),
																		 show_progress = T) {

	if (show_progress) {
		iter_fun <- pblapply
	} else {
		iter_fun <- lapply
	}

	if ('Target' %nin% colnames(data)) {
		data$Target <- coalesce_labels(data)
	}

	if (!is.null(test_data)) {
		data <- import_classification(data, prev_records = test_data)
	}

	data <- data %>%
		select(any_of(c('ID', 'Target', 'Rev_previous', 'Pred_Med', 'Pred_Low',
										'Pred_Up', 'Predicted_label'))) %>%
		mutate(Set = ifelse(!is.na(Target), 'Train', 'Test'))

	obs <- coalesce_labels(data, c('Rev_previous', 'Target'))
	if (is.null(truePos)) truePos <- sum(obs %in% 'y', na.rm = T)
	if (is.null(trueNeg)) trueNeg <- sum(obs %in% 'n', na.rm = T)
	preds <- coalesce_labels(data, c('Rev_previous', 'Target',
																	 'Predicted_label'))
	predPos <- sum(preds %in% 'y', na.rm = T)
	predNeg <- sum(preds %in% 'n', na.rm = T)
	rm(obs, preds)

	if (!is.null(samples)) {
		if (nrow(data) != nrow(samples)) {
			stop('Data and posterior samples have a different number of rows')
		}
		data$Samples <- left_join(data[,'ID'], samples, by = "ID")
		#data$Samples <- samples[match(data$ID, samples$ID),]
	} else data$Samples <- data$Pred_Med

	iter_fun(list('Train', 'Test', c('Train', 'Test')), function(set) {
		data <- data %>%
			mutate(
				Target_test = coalesce_labels(cur_data(), c('Rev_previous', 'Target'))
			) %>%
			filter(Set %in% set)

		set <- paste(if (length(set) == 1) set else 'Total')

		if (nrow(data) == 0) return(data.frame(
			Value = c(glue('{set} (n = {nrow(data)})'), rep(NA, 9)) # Very ugly to put this number manually
		))

		data %>%
			filter(!is.na(Target_test)) %>%
			summarise(
				Set = glue('{set} (n = {n()})'),

				'AUC [CrI]' =  if (n_distinct(Target_test) == 1) {
					'One class only'
				} else {
					as.data.frame(cur_data()$Samples) %>%
						select(-any_of('ID')) %>%
						mclapply(function(p) {

							pROC::roc(response = Target_test, predictor = p) %>% #suppressMessages() %>%
								pROC::auc() %>% as.vector()
						}) %>% unlist() %>% {
							if (length(.) > 1) {
								quantile(., perf_quants) %>% percent() %>%
									{glue("{.[2]} [{.[1]}, {.[3]}]")}
							} else percent(.)
						}
				},

				'SetLimits, pos/neg' = if ('Train' %in% set) {
					if (is.null(negLim)) {
						negLim <- max(Pred_Up[Target %in% 'n'])
					}

					if (is.null(posLim)) {
						posLim <- min(Pred_Low[Target %in% 'y'])
					}

					c(posLim, negLim) %>%
						percent() %>% paste(collapse = '/')
				} else NA,

				'Predicted, n (%) [y/n]' = sapply(c('y', 'unk', 'check', 'n'), function(outc) {
					abs <- sum(data$Predicted_label == outc) # This include also non
					# labeled records, therefore it needs to be namespaced

					perc <- percent(abs / nrow(data))
					pos <- sum(Target_test[Predicted_label == outc] == 'y')
					neg <- sum(Target_test[Predicted_label == outc] == 'n')

					glue('{outc}: {abs} ({perc}) [{pos}/{neg}]')
				}) %>% paste(collapse = ', '),

				'Observed, n (%)' = summarise_vector(Target_test),

				'Sens., pot. (act.)' = {
					x <- c(mean(Predicted_label[Target_test == 'y'] != 'n'), # potential
								 mean(Predicted_label[Target_test == 'y'] == 'y') # actual
					) %>% percent()
					glue('{x[1]} ({x[2]})')
				},

				'Spec., pot. (act.)' = {
					x <- c(mean(Predicted_label[Target_test == 'n'] != 'y'), # potential
								 mean(Predicted_label[Target_test == 'n'] == 'n') # actual
					) %>% percent()
					glue('{x[1]} ({x[2]})')
				},

				# Samples %>% select(-ID) %>% pblapply(function(x) {
				# 	Predicted_label = case_when( # assign y if a record range is included into global y range and don't overlap the n range. The opposite is true for n labels
				# 		x > negLim & x > posLim ~ 'y',
				# 		x < posLim & x < negLim ~ 'n',
				# 		T ~ 'unk' # Assign unk if the label is not clearly defined
				# 	)
				#
				# 	Predicted_label = replace(
				# 		Predicted_label,
				# 		Predicted_label != Target & Predicted_label != 'unk',
				# 		'check')
				#
				# 	mean(Predicted_label[Target_test == 'n'] == 'n')
				# }) %>% unlist %>% quantile(perf_quants)

				PPV = percent(mean(
					Target_test[Predicted_label == 'y' |
												(Predicted_label == 'unk' & Target_test == 'y')] == 'y')),
				NPV = percent(mean(
					Target_test[Predicted_label == 'n' |
												(Predicted_label == 'unk' & Target_test == 'n')] == 'n')),

				## Similar results to the next indicators, but have finite bounds (no divisions by zero)
				# 'Pos per sample rate [CrI] (prob. > random)' = {
				# 	obsPos <- sum(Target_test %in% 'y')
				# 	obs <- (obsPos/qhyper(perf_quants, truePos, trueNeg, n())) %>%
				# 		signif(3)
				# 	obs_p <- phyper(obsPos, truePos, trueNeg, n()) %>% percent()
				#
				# 	pred <- (obsPos/qhyper(perf_quants, predPos, predNeg, n())) %>%
				# 		signif(3)
				# 	pred_p <- phyper(obsPos, predPos, predNeg, n()) %>% percent()
				#
				# 	glue('test: {obs[2]} [{obs[3]}, {obs[1]}] ({obs_p}), pred: {pred[2]} [{pred[3]}, {pred[1]}] ({pred_p})')
				# },

				'Random samples needed [CrI] (prob. < random)' = {
					qnhyper <- extraDistr::qnhyper
					pnhyper <- extraDistr::pnhyper

					obsPos <- sum(Target_test %in% 'y')
					obs <- (qnhyper(perf_quants, trueNeg, truePos, obsPos) / n()) %>%
						signif(3) %>% sort()
					obs_p <- (1 - pnhyper(n(), trueNeg, truePos, obsPos)) %>% percent()

					pred <- (qnhyper(perf_quants, predNeg, predPos, obsPos) / n()) %>%
						signif(3) %>% sort()
					pred_p <- (1 - pnhyper(n(), predNeg, predPos, obsPos)) %>% percent()

					glue('test: {obs[2]} [{obs[1]}, {obs[3]}] ({obs_p}), pred: {pred[2]} [{pred[1]}, {pred[3]}] ({pred_p})')
				},

				## https://doi.org/10.1186/s13643-016-0263-z
				## Not intuitive to explain
				# WWS = {
				# 	TN = sum(Predicted_label == 'n' & Target_test == 'n')
				# 	FN = sum(Predicted_label == 'n' & Target_test == 'y')
				# 	N = sum(!is.na(Target_test))
				# 	Sens = mean(Predicted_label[Target_test == 'y'] != 'n')
				#
				# 	(TN + FN) / N - (1 - Sens)
				# } %>% percent
			) %>%
			tidyr::pivot_longer(everything(), names_to = 'Statistic', values_to = set) %>% {
				if (!identical(set, 'Train')) .[,-1] else .
			}
	}) %>%
		bind_cols() #%>%
	#setNames(c('Statistic', 'Train', 'Test', 'Total'))

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
						glue('{.x} ({percent(.x/nrow(Annotations))})')
					} else NA),
					.after = matches('Target')
				)
		}
}




enrich_annotation_file <- function(file, session_name, DTM = NULL,
																	 ## Model parameters
																	 pos_mult = 10,
																	 n_models = 10,
																	 resample = F,
																	 perf_quants = c(.01, .5, .99),
																	 #
																	 sessions_folder = 'Sessions',
																	 autorun = T, replication = NULL,
																	 dup_session_action = c('fill', 'add',
																	 											 'replace', 'stop'),
																	 limits = list(
																	 	stop_after = 4, pos_target = NULL,
																	 	labeling_limit = NULL
																	 ),
																	 compute_performance = T,
																	 test_data = NULL,
																	 use_prev_labels = T,
																	 prev_classification = NULL,
																	 rebuild = FALSE, ...) {

	process_id <- paste0('.pID__', str_replace_all(file, fixed(.Platform$file.sep), '__'))

	if (file.exists(process_id)) {
		message('File already being processed. Skipping.')
		return(NULL)
	}

	writeLines(paste('Process started on:', safe_now()), process_id)

	on.exit(file.remove(process_id))

	if (length(perf_quants) != 3 & any(perf_quants >= 1 | perf_quants <= 0)) {
		stop('"perf_quants" should be 3 quantiles, between 0 and 1 (included).')
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

	perf_quants <- sort(perf_quants)[c(2, 1, 3)]

	dup_session_action <- match.arg(dup_session_action)

	dup_session_action <- if (dup_session_action == 'fill') 'skip' else dup_session_action

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

	# Coalesce Rev_prediction_new into Rev_prediction and clear the former
	Records <- Records %>%
		mutate(
			Rev_prediction = coalesce_labels(., c('Rev_prediction_new', 'Rev_prediction')),
			Rev_prediction_new = NA,
			.after = Rev_prediction
		)

	if ('*' %in% Records$Rev_prediction) stop('There are unreviewed predictions.')
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
	if (is.null(DTM)) {
		DTM <- file.path(sessions_folder, session_name, 'DTM.rds')
	}

	# Reload DTM if existing and there were no new positive matches
	if (is.character(DTM) && file.exists(DTM) & repl > 1) {
		message('Loading DTM')

		DTM <- read_rds(DTM)

	} else {
		message('Creating DTM')

		DTM <- create_training_set(Records)
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
		# 		# 	quants = perf_quants),
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

			preds <- bart_machine_get_posterior(
				bart.mod,
				new_data = DTM %>% select(all_of(colnames(bart.mod$X)))
			)$y_hat_posterior_samples

			if (!exists('Samples')) {
				Samples <- preds
			} else {
				Samples <- Samples + preds
			}

			Var_imp[[i]] <- bartMachine::get_var_props_over_chain(bart.mod, 'trees')

			rm(bart.mod, preds, train_data)

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
			apply(Samples[,-1], 1, quantile, perf_quants) %>% t %>%
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
		message('Adding performance summary')

		if (!is.null(Test_data)) {

			tictoc::tic()

			Performance <- compute_pred_performance(Annotated_data, samples = Samples,
																							test_data = Test_data,
																							perf_quants = perf_quants)
			tictoc::toc()
		}
	}

	## Prints the performance summaries out, but it's a bit messy and this data is already in excel
	# Performance[,-1] %>%
	# 	lapply(function(perf) setNames(as.character(perf), Performance[[1]])) %>%
	# 	print()



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
		summarise(Value = mean(Val), Score = if (n_models > 1) Value / sd(Val) else Value, n_models = n())

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

	message('- DTM...')
	tictoc::tic()
	DTM_file <- file.path(session_path, 'DTM.rds')

	readr::write_rds(DTM, file = DTM_file, compress = 'gz')

	tictoc::toc()

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

	message('- posterior samples...')
	tictoc::tic()

	output_file_samp <- file.path(session_path, 'Samples',
																glue('{iter}.Samples_{common_tag}.rds'))
	dir.create(dirname(output_file_samp), showWarnings = F, recursive = T)

	readr::write_rds(Samples, file = output_file_samp, compress = 'gz')

	tictoc::toc()

	if (autorun) {
		if ('*' %nin% Annotated_data$Rev_prediction_new) {
			message('\n\nAutomatic restart')

			rm(Predicted_data, Annotated_data, Var_imp, Samples)
			gc()

			if (file.exists('Model_backup.rds')) file.remove('Model_backup.rds')

			file.remove(process_id)

			enrich_annotation_file(output_file_ann, DTM = DTM_file,
														 ## Model parameters
														 pos_mult = pos_mult,
														 n_models = n_models,
														 perf_quants = perf_quants,
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

perform_grid_evaluation <- function(records, sessions_folder = 'Grid_Search',
																		prev_classification = records,
																		## Model parameters
																		resample = c(FALSE, TRUE),
																		n_init = c(50, 100, 250, 500),
																		n_models = c(5, 10, 20, 40, 60),
																		pos_mult = c(1, 10, 20),
																		perf_quants = list(c(.1, .5, .9),
																											 c(.05, .5, .95),
																											 c(.01, .5, .99)),
																		## Passed arguments
																		rebuild = TRUE,
																		limits = list(
																			stop_after = 4,
																			pos_target = NULL, labeling_limit = NULL
																		)) {

	# file: 'Sessions/Session1/Records_2021-03-18T13.09.24.xlsx'

	# prepare the parameter grid
	Grid <- tidyr::expand_grid(
		resample, n_init, n_models, pos_mult, perf_quants
	) %>%
		mutate(
			iteration = glue('{1:n()} / {n()}'),
			session = paste(
				'GridSession',
				glue('{n_models}Mods'),
				glue('{ifelse(resample, "y", "n")}Resamp'),
				glue('{n_init}Init'),
				glue('{pos_mult}Mult'),
				glue('{(sapply(perf_quants, max)-sapply(perf_quants, min))*100}Quant'),
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
		if (length(list.files(session_path, pattern = 'Records', recursive = TRUE)) == 0) {
			Records <- Records %>%
				# remove labels in excess of "n_init"
				mutate(across(
					any_of(c('Rev_manual', 'Rev_prediction', 'Rev_prediction_new')),
					~ replace(.x, Order > Grid[i,]$n_init, NA)
				))

			create_session(Records, session_name = Grid[i,]$session,
																		 sessions_folder = sessions_folder,
																		 dup_session_action = 'replace')
		}

		# pick the last annotated record file or the source one if any
		last_record_file <- tibble(
			path = list.files(session_path, recursive = T) %>% str_subset('Records'),
			iter = str_remove(path, 'Annotations.') %>% # the dot stands for the file system separator
				str_extract('^\\d+') %>%
				as.numeric() %>%
				pmax(0, na.rm = T) # the source record file would have no iteration in the name, so will be considered as zero
		) %>% with(path[iter == max(iter)])

		last_record_file <- file.path(session_path, last_record_file)

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
				 								 perf_quants = perf_quants[[1]])
		)
	}) %>% unlist() %>% table()
}

analyse_grid_search <- function(session_folder = 'Grid_Search', tot_pos = NULL,
																tot_records = NULL,  plot = TRUE,
																score = c('Sens_adj_eff', 'Pos_rate_adj_sens',
																					'Pos_rate')) {

	score <- match.arg(score)

	if (is.null(tot_pos) | is.null(tot_records)) {
		Labels <- list.files(session_folder, pattern = 'Records_', recursive = T, full.names = T)[1] %>%
			read_excel() %>%
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
			Pos_rate_adj_sens = Pos_rate * (Pos_labels / tot_pos),
			Sens_adj_eff = (Pos_labels / tot_pos) * (1 - Tot_labeled / tot_records),
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

	if (plot) {

		p <- lapply(params, function(par) {
			params <- setdiff(params, par)
			out %>%
				mutate(group = select(cur_data(), one_of(params)) %>%
							 	apply(1, paste, collapse = ' ')) %>%
				ggplot(aes(factor(get(par)), Score)) +
				geom_boxplot(show.legend = F, outlier.shape = NA) +
				geom_boxplot(aes(fill = Rule), show.legend = F, outlier.shape = NA) +
				geom_line(aes(group = group, color = Rule), alpha = .5, show.legend = F) +
				geom_point(aes(color = Rule), show.legend = F) +
				theme_minimal() +
				scale_y_continuous(labels = function(x) round(x, 2)) +
				ylab(score) +
				xlab(par)
		})

		p_one <- out %>%
			ggplot(aes(factor(get(params[1])), Score)) +
			geom_line(aes( group = Rule, color = Rule), size = 2) +
			theme_minimal() +
			labs(color = glue('Par. group (mean {score})'))

		tmp <- ggplot_gtable(ggplot_build(p_one))
		leg <- which(tmp$layout$name == 'guide-box')

		legend <- tmp$grobs[[leg]]

		p <- patchwork::wrap_plots(p) + legend

		print(p)
	}

	list(
		iterations = out,
		best_parms = out %>% filter(str_detect(Rule, '^1\\.')) %>%
			slice_max(Pos_labels, n = 1, with_ties = T) %>%
			slice_max(Score, n = 1, with_ties = F) %>%
			select(Iter, Rep, Tot_labeled, Pos_labels, Score, any_of(params)) %>%
			mutate(
				Score = glue("{signif(Score, 3)} ({score})"),
				Pos_labels = glue("{Pos_labels} / {tot_pos}"),
				Tot_labeled = glue("{Tot_labeled} / {tot_records}"),
				across(.fns = as.character)) %>%
			tidyr::pivot_longer(everything(), names_to = 'Parameter', 'Value')
	)
}

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
