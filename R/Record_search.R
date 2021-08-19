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
											 default_field = 'TS', api_key = options('baysren.wos_api_key')[[1]],
											 parallel = T, parse_query = T, ...) {

	message('Searching WOS...')

	if (parallel) { ## Use mclapply which is faster
		pull_records <- function (query, editions = c("SCI", "SSCI", "AHCI", "ISTP",
																									"ISSHP", "BSCI", "BHCI", "IC", "CCR", "ESCI"), sid = auth(Sys.getenv("WOS_USERNAME"),
																																																						Sys.getenv("WOS_PASSWORD")), ...)
		{
			parse_wos <- function(all_resps)
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
													api_key = options('baysren.ncbi_api_key')[[1]],
													record_limit = numeric(),
													...) {

	message('Searching Pubmed...')

	if (is.null(api_key)) warning('NCBI API key is not set.')

	query <- str_squish(query)

	year_query <- clean_date_filter_arg(year_query, cases = list(
		gt = '{year_piece + 1}[PDAT]:{year(today())}[PDAT]',
		ge = '{year_piece}[PDAT]:{year(today())}[PDAT]',
		eq = '{year_piece}[PDAT]:{year_piece}[PDAT]',
		le = '1000[PDAT]:{year_piece}[PDAT]',
		lt = '1000[PDAT]:{year_piece - 1}[PDAT]',
		range = '{year_piece[1]}[PDAT]:{year_piece[2]}[PDAT]'),
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
												api_key = options('baysren.ieee_api_key')[[1]], allow_web_scraping = T,
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

		if (!is.null(data$keywords)) {
		Keys <- data$keywords %>%
			group_by(type = case_when(
				str_detect(type, 'MeSH') ~ 'Mesh',
				str_detect(type, 'Author') ~ 'Author',
				T ~ 'IEEE'
			)) %>%
			summarise(kwd = paste(unlist(kwd), collapse = '; '))

		Keys <- setNames(as.list(Keys$kwd), Keys$type)
		} else {
			Keys = data.frame(IEEE = NA, Mesh = NA, Author = NA)
		}

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
