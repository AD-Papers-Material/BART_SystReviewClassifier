
all(
	(model* OR (mathematical model*) OR network) AND
	(disseminat* OR transmission OR spread OR diffusion) AND
	(
		(
			(nosocomial OR "healthcare associated" OR "hospital acquired" OR "long term care" OR "longterm care") AND infection
		) OR
		((antimicrobial OR "anti infective") AND resistan*)
	)
)

((model OR models OR modeling OR network OR networks) AND (dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR hospital OR "long-term-care" OR "long term care" OR "longterm care" OR "long-term care" OR "hospital acquired" OR "healhtcare associated") AND (infection OR resistance OR resistant)) AND (2010[PDAT]:2020[PDAT])

str_extract_all(x$Abstract, paste(terms, collapse = '|') %>% str) %>% unlist %>% unique

terms <- str_remove_all('((model OR models OR modeling OR network OR networks) AND (dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR hospital OR "long-term-care" OR "long term care" OR "longterm care" OR "long-term care" OR "hospital acquired" OR "healhtcare associated") AND (infection OR resistance OR resistant))', '[^\\w\\s\\*]+|(?<= )(AND|OR)(?= )') %>% str_split('\\s+') %>% unlist %>% unique %>% str_replace_all('\\*', '\\\\w*')

transmute(x, Rev_title, Rev_abstract, Rev_prediction, Predicted_label, Pred_Med, Target = case_when(
	!is.na(Rev_prediction) ~ Rev_prediction,
	!is.na(Rev_abstract) ~ Rev_abstract,
	T ~ Rev_title
) %>% factor, text = paste(Title, Abstract), doc.length = str_count(text, '\\b'),  term.count = str_count(text, paste(terms, collapse = '|')), score = term.count/doc.length, beta.med = qbeta(.5, term.count + 1.1, doc.length - term.count + 1.1), beta.high = qbeta(.95, term.count + 1.1, doc.length - term.count + 1.1), beta.low = qbeta(.05, term.count + 1.1, doc.length - term.count + 1.1), beta.std = beta.med / (beta.high - beta.low)) %>% arrange(desc(score)) %>% mutate(i = 1:n()) %>% View


term.count
Target FALSE TRUE
n   932  144
y    27   46
score
Target FALSE TRUE
n   892  184
y    16   57
tfidf
Target FALSE TRUE
n   901  175
y    29   44
beta.med
Target FALSE TRUE
n   895  181
y    16   57
beta.std
Target FALSE TRUE
n   911  165
y    22   51


# A tibble: 2 x 5
term                                     estimate std.error statistic  p.value
<chr>                                       <dbl>     <dbl>     <dbl>    <dbl>
	1 (Intercept)                                 -3.54     0.195    -18.1  1.50e-73
2 I(term.count > sort(term.count, T)[1000…     2.40     0.258      9.29 1.58e-20
																		 # A tibble: 2 x 5
																		 term                                estimate std.error statistic  p.value
																		 <chr>                                  <dbl>     <dbl>     <dbl>    <dbl>
																		 	1 (Intercept)                            -4.02     0.252    -15.9  3.16e-57
																		 2 I(score > sort(score, T)[1000])TRUE     2.85     0.294      9.68 3.58e-22
																		 # A tibble: 2 x 5
																		 term                                estimate std.error statistic  p.value
																		 <chr>                                  <dbl>     <dbl>     <dbl>    <dbl>
																		 	1 (Intercept)                            -3.44     0.189    -18.2  4.01e-74
																		 2 I(tfidf > sort(tfidf, T)[1000])TRUE     2.06     0.253      8.12 4.53e-16
																		 # A tibble: 2 x 5
																		 term                                     estimate std.error statistic  p.value
																		 <chr>                                       <dbl>     <dbl>     <dbl>    <dbl>
																		 	1 (Intercept)                                 -4.02     0.252    -16.0  2.53e-57
																		 2 I(beta.med > sort(beta.med, T)[1000])TR…     2.87     0.294      9.74 1.94e-22
																		 # A tibble: 2 x 5
																		 term                                     estimate std.error statistic  p.value
																		 <chr>                                       <dbl>     <dbl>     <dbl>    <dbl>
																		 	1 (Intercept)                                 -3.72     0.216    -17.3  9.76e-67
																		 2 I(beta.std > sort(beta.std, T)[1000])TR…     2.55     0.269      9.49 2.39e-21





search_ieee <- function(query, year_query = NULL, additional_fields = NULL,
												api_key = options('ieee_api_key'), allow_web_scraping = F,
												query_name = glue('Pubmed_{safe_now()}'), save = T) {

	query <- str_squish(query)

	year_arg <- clean_date_filter_arg(year_query, cases = list(
		gt = '{year_piece + 1}_{year(today())}',
		ge = '{year_piece}_{year(today())}',
		eq = '{year_piece}_{year_piece}',
		range = '{year_piece[1]}_{year_piece[2]}',
		le = '1900_{year_piece}',
		lt = '1900_{year_piece - 1}'),
		arg_in_query_test = '_Year', query = query)

	year_query <- glue('({year_query})')

	default_fields <- c(
		contentType = 'periodicals',
		highlight = 'false',
		returnFacets = 'ALL',
		returnType = 'json',
		matchPubs = 'true'
	)

	if (!is.null(additional_fields) & length(additional_fields) > 0) {

		additional_fields <- unlist(additional_fields)

		additional_fields <- case_when(
			additional_fields == 'TRUE' ~ 'true',
			additional_fields == 'FALSE' ~ 'false',
			T ~ additional_fields
		)

		default_fields <- default_fields[
			names(default_fields) %nin% names(additional_fields)
			]
	}

	additional_fields <- c(default_fields, additional_fields)

	if (is.null(api_key[[1]])) {
		warning('IEEE API key is not set')

		if (!allow_web_scraping) stop('If API key is not present web scraping must be allowed.')


		entry_point <- 'https://ieeexplore.ieee.org/search/searchresult.jsp?'
		args <- c(query = URLencode(query), additional_fields)

		arg_query <- paste(glue('{names(args)}={args}'), collapse = '&')

		url <- paste(entry_point, arg_query)

		response <- get_website_resources(url = url, url_filter = 'rest/search',
																	type_filter = 'XHR')[[1]]

		jsonlite::fromJSON(resp[[1]]$response$body)$records %>%
			transmute(
				Order = 1:n(),
				ID = paste0('IEEE:', articleNumber),
				Title = articleTitle,
				Abstract = NA, Keywords, Mesh,
				DOI = doi, URL = paste0('https://ieeexplore.ieee.org/document/', articleNumber),
				Authors = authors %>% sapply(function(df) paste(df$normalizedName, collapse = '; ')),
				Journal = publicationTitle,
				N_citations = citationCount,
				Published = publicationDate,
				Source = 'IEEE',
				File = query_name
			)


		%>% mutate(
				across(where(is.character), ~ replace(.x, .x == '', NA)),
				across(where(is.character), ~ str_squish(.x) %>% str_replace_all(' +;', ';'))
			)
	}


	entries %>% transmute(
		Order = 1:n(),
		ID = paste0('IEEE:', str_remove(`PDF Link`, fixed('https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber='))),
		Title = `Document Title`,
		Abstract, DOI, URL = `PDF Link`,
		Authors, Journal = `Publication Title`,
		Author_keywords = `Author Keywords`,
		Article_type = str_remove(`Document Identifier`, 'IEEE '),
		Keywords = cbind(`IEEE Terms`, `INSPEC Controlled Terms`, `INSPEC Non-Controlled Terms`) %>%
			apply(1, function(x) if (any(!is.na(x))) paste(na.omit(x), collapse = ';') else NA),
		Mesh = Mesh_Terms,
		N_citations = `Article Citation Count`,
		Published = `Online Date`,
		Source = 'IEEE',
		File = basename(file)
	) %>% mutate(across(where(is.character), str_squish))
}
}) %>% setNames(basename(files))





	resp <- get_website_resources('https://ieeexplore.ieee.org/search/searchresult.jsp?contentType=periodicals&queryText=((model%20OR%20models%20OR%20modeling%20OR%20network%20OR%20networks)%20AND%20(dissemination%20OR%20transmission%20OR%20spread%20OR%20diffusion)%20AND%20(nosocomial%20OR%20hospital%20OR%20%22long-term-care%22%20OR%20%22long%20term%20care%22%20OR%20%22longterm%20care%22%20OR%20%22long-term%20care%22%20OR%20%22healthcare%20associated%22)%20AND%20(infection%20OR%20resistance%20OR%20resistant))&highlight=true&returnFacets=ALL&returnType=json&matchPubs=true&ranges=2010_2020_Year', url_filter = 'rest/search', type_filter = 'XHR')

	(resp$`interception-job-69.0`$response$body %>% jsonlite::fromJSON())$records

	f <- read_file('view-source:https://ieeexplore.ieee.org/document/5970118')
	extr <- str_extract(f, '(?<=xplGlobal\\.document\\.metadata=).+')
	extr %>% str_remove(';$') %>% jsonlite::fromJSON() %>% pull(abstract)
}


