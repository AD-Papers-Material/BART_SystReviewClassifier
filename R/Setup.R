
# Initialize --------------------------------------------------------------


Sys.setenv(LANG = "en")

if (is.null(getOption('BartMem')) & interactive()) {
	local({
		mem <- readline("How much GB of memory should be used (better no more than 90% of available one)?")

		if (is.na(as.numeric(mem))) stop('Input should be a number.')

		mem <- paste0("-Xmx", mem, "g")

		options(BartMem = mem)
	})
}

options(java.parameters = getOption('BartMem'))

pkg.require <- function(install_and_load = c(), only_install = c()) {

	if (!('devtools' %in% installed.packages())) install.packages('devtools')

	all_packages <- unique(c(install_and_load, only_install))
	all_packages_clean <- sub(pattern = '^.*/', replacement = '', x = all_packages)

	only_install <- all_packages %in% only_install
	is_github <- grepl('/', all_packages)
	not_installed <- !(all_packages_clean %in% installed.packages()[,1])

	# Install
	sapply(all_packages[!is_github & not_installed], install.packages)
	sapply(all_packages[is_github & not_installed], devtools::install_github)

	# Load
	sapply(all_packages_clean[!only_install], library, character.only = TRUE)
}

pkg.require(
	install_and_load = c("dplyr", "stringr", "glue", "readr", "readxl", "lubridate", "Matrix", "igraph", "pbapply",
											 "pbmcapply", "rpart", "bartMachine", "tm", "patchwork", "ggplot2", "ggrepel", "RLesur/crrri",
											 "bakaburg1/tidytrees"),
	only_install = c('purrr', 'openxlsx', 'tictoc', 'tidyr', 'arm', 'english',
									 'parallel', 'jsonlite', 'rentrez', 'wosr', 'brms', 'ggridges',
									 'knitr', 'kableExtra', 'extraDistr')
)


# Windows does not support mclapply, regressing to lapply
if (.Platform$OS.type != 'unix') {
	warning('Forked parallel operations not allowed on Windows. Falling back to sequential.')

	mclapply <- lapply
	pbmclapply <- pblapply
}

if (bart_machine_num_cores() != parallel::detectCores()) {

	message('Using ', parallel::detectCores(), ' cores')
	invisible(capture.output(set_bart_machine_num_cores(parallel::detectCores())))
	options(mc.cores = parallel::detectCores())
}

# Setting up other general parameters for the framework
options(baysren.probs = c(.05, .5, .95))
options(baysren.sessions_folder = 'Sessions')


# Helper functions --------------------------------------------------------


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


# Load the infrastructure -------------------------------------------------

if (file.exists('secrets.R')) source('secrets.R')

local({
	for (file in file.path('R', list.files('R') %>% str_subset('Setup', negate = T))) {
		source(file)
	}
})

