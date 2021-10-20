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
		Source_type = 'parsed',
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

			return(import_data(file))
		}

		message('Parsing ', basename(file), '...')

		type <- NULL

		if (str_detect(file, '\\.(nbib|txt)$')) {
			entries <- read_file(file)

			if (str_detect(entries, 'PMID-')) type <- 'medline'

		} else if (str_detect(file, '\\.(xlsx?|csv)$')) {
			entries <- import_data(file)

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
	terms <- str_remove_all(query, "NOT ?(\\w+|\\(.*?\\))") %>%
		str_remove_all('[^\\w\\s\\*]+|(?<= )(AND|OR)(?= )') %>%
		str_split('\\s+') %>%
		unlist() %>% unique() %>%
		str_replace_all('\\*', '\\\\w*') %>%
		Filter(function(x) str_length(x) > 2, .)

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
																	 prev_classification = NULL) {

	if (class(records) %nin% c('character', 'list', 'data.frame')) {
		stop('"records" should be either of vector of file/folder paths, a list of data.frame or a single data.frame')
	}

	if (is.character(records)) {
		records <- c(
			list.files(records, full.names = T, recursive = T) %>%
				str_subset('~\\$', negate = T) %>%
				str_subset('(parsed|API)\\.csv'),
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

		records <- import_classification(records, prev_records = prev_classification)
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
													 sessions_folder = getOption("baysren.sessions_folder"),
													 DTM = NULL,
													 dup_session_action = c('skip', 'stop', 'add', 'replace', 'silent'),
													 use_time_stamp = TRUE) {

	message("Creating session: ", session_name)

	dup_session_action <- match.arg(dup_session_action)

	initialise_session <- function(Records, session_path, DTM = NULL,
																 use_time_stamp = T) {

		if (use_time_stamp) ts <- glue('_{safe_now()}') else ''

		# Create the session folder
		if (!dir.exists(session_path)) {
			message('- create session folder "', session_path, '".')
			dir.create(session_path, recursive = T, showWarnings = FALSE)
		}

		# At the moment csv files will be converted to excel, eventually both file
		# type will be supported
		if (is.character(Records) && str_detect(Records, '\\.csv$')) Records <- import_data(Records)

		message("- copy or write the Record data")

		file_path <- file.path(session_path, glue('Records{ts}.xlsx'))
		if (is.character(Records) | is.factor(Records)) {
			if (!file.exists(Records)) stop(Records, ' does not exists.')

			file.copy(Records, file_path, overwrite = T, recursive = F)
		} else {
			openxlsx::write.xlsx(Records, file = file_path, asTable = T)
		}

		message("- Copy or write the DTM data")
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
					 silent = {
					 	return(session_path)
					 },
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
	}

	initialise_session(Records, session_path, DTM, use_time_stamp)

	return(session_path)
}

get_session_files <- function(session_name,
															sessions_folder = getOption("baysren.sessions_folder"),
															which = c('Records', 'Annotations',
																				'DTM', 'Samples', 'Results')) {

	session_path <- file.path(sessions_folder, session_name)

	lapply(which, function(type) {
		files <- list.files(session_path, recursive = T) %>% str_subset(type)

		files <- files[str_detect(basename(files), '^\\w')]

		if (type == 'Records') {
			files <- files[!str_detect(files, 'Annotations')]
		}

		if (length(files) == 0) return(NULL)

		files <- tibble(
			files,
			iter = basename(files) %>%
				str_extract('^\\d+') %>%
				as.numeric() %>%
				pmax(0, na.rm = T) # the source record file would have no iteration in the name, so will be considered as zero
		) %>% arrange(iter) %>% pull(files)

		file.path(session_path, files)
	}) %>% setNames(which)
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
			## TODO: clean up is necessary for Source and Source_type
			N_citations = suppressWarnings(na.omit(N_citations) %>% max(na.rm = T) %>% purrr::modify_if(~ !is.finite(.x), ~ NA))
		)

	bind_rows(unique_sources, dup_sources) %>% select(-UID) %>%
		clean_record_textfields() %>%
		filter(!duplicated(ID))
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

	# Join manual classifications in one target column
	if (is.null(column)) {
		records <- records %>%
			mutate(Target = coalesce_labels(., c('Rev_prediction', 'Rev_manual')))
	} else records$Target <- records[[column]]

	records <- records %>% arrange(Order) %>%
		filter(!is.na(Target))

	# Define plot breaks according to a limit of reviewed records
	if (is.null(limit)) limit <- max(which(!is.na(records$Target)))
	steps <- seq(step_size, limit, by = step_size) %>% c(limit) %>% unique()

	# Count positive and negative matches in every break
	df <- pblapply(steps, function(step) {
		records %>% head(step) %>%
			summarise(
				Yes = sum(Target == 'y', na.rm = T),
				No = sum(Target == 'n', na.rm = T)
			)
	}) %>% bind_rows()

	# Plot trends
	p <- df %>%
		ggplot(aes(x = steps)) +
		geom_line(aes(y = Yes, color = 'yes'), size = 1) +
		geom_line(aes(y = No, color = 'no'), size = 1) +
		labs(y = 'Records', x = 'Batch size', color = 'Classification') +
		theme_minimal()

	# Remove consecutive non changing values to avoid label cluttering
	df <- mutate(
		df,
		across(c(Yes, No), function(x) {
			c(x[1], sapply(2:(n() - 1), function(i) {
				if (x[i] == x[i-1]) NA else x[i]
			}), x[n()]) })
	)

	# Add labels
	p +
		geom_label(aes(y = Yes, x = steps, label = Yes), data = df, alpha = .8) +
		geom_label(aes(y = No, x = steps, label = No), alpha = .8)
}
