
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
									 'knitr', 'kableExtra', 'extraDistr', 'broom', 'viridis', 'lexicon')
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


# Load the infrastructure -------------------------------------------------

if (file.exists('secrets.R')) source('secrets.R')

local({
	for (file in file.path('R', list.files('R') %>% str_subset('Setup', negate = TRUE))) {
		source(file)
	}
})

