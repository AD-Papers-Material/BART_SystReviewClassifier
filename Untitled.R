
run_search_query <- function(query, year_query = NULL, actions = c('API', 'parsed')
														 sources = c('IEEE', 'WOS', 'Pubmed'),
														 session_name = 'Session1', query_name = 'Query1',
														 records_folder = 'Records', overwrite = F,
														 session_journal = 'Session_journal.csv') {

	actions <- match.args(actions)
	sources <- match.args(sources)

	folder_path <- file.path(records_folder, session_name, query_name)

	if (!dir.exists(folder_path)) dir.create(folder_path, recursive = T)

	if (!file.exists(query_journal)) {
		type <- str_extract(query_journal, '(csv|xlsx?)$')

		if (str_detect(query_journal, '\\.csv$')) {

		} else if (str_detect(query_journal, '\\.xlsx?$')) {


		} else stop('Query journal file type must be csv or excel.')

	}

	query_ts <- safe_now()

	query_data <- pblapply(sources, function(source) {

		input_file <- NA
		output_file <- file.path(folder_path, glue('{source}_{action}.csv'))

		lapply(actions, function(action) {
			if (file.exists(output_file) & !overwrite) {
				warning(output_file, ' already present and argument overwrite == FALSE.')

				return(NULL)
			}

			if (file.exists(output_file) & overwrite) warning(output_file, ' will be overwritten.')

			if (action == 'API') {
				## API search

				search_fun <- get(parse0('search_', str_to_lower(source)))


				records <- search_fun(query = query, year_query = year_query,
															file_name = output_name, save = F) %>%
					mutate(File = output_name, Session = session_name, Query = query_name)

				write_csv(records, output_file)

			} else if (action == 'parsed') {
				## Parsing downloaded records
				input_file <- list.files(folder_path, full.names = F) %>%
					str_subset(paste(actions, collapse = '|'), negate = T) %>%
					str_subset(regex(source, ignore_case = T))

				input_file <- file.path(folder_path, input_file)

				if (length(input_file) > 0) {
					if (length(input_file) > 1) stop('Only one downloaded file to parse can exists per source/query/session.')

					records <- read_bib_files(input_file)
				}
			}

			data.frame(
				Session_ID = session_name,
				Query_ID = query_name,
				DB = source,
				Type = action,
				Source_file = basename(input_file),
				Parsed_file = basename(output_file),
				Timestamp = query_ts,
				Filter = year_query,
				N_results = nrow(records),
				Query = query
			)
		})

	})

}
