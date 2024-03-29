% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Record_search.R
\name{search_pubmed}
\alias{search_pubmed}
\title{Automatic search on Pubmed database}
\usage{
search_pubmed(
  query,
  year_query = NULL,
  additional_fields = NULL,
  api_key = getOption("baysren.ncbi_api_key"),
  record_limit = numeric()
)
}
\arguments{
\item{query}{A boolean query with AND/OR/NOT operators, brackets for term
grouping and quotation marks for n-grams.}

\item{year_query}{A year based filtering query. See
\code{\link{clean_date_filter_arg}} for more info.}

\item{additional_fields}{Additional fields to add to the query. Will not be
normalized, so it must already follow Pubmed specific syntax.}

\item{api_key}{Not mandatory but is helpful when performing searches with a
large number of results to avoid quota limitations.}

\item{record_limit}{A limit on the number of records collected.}
}
\value{
A data frame of records.
}
\description{
Perform an API search using Pubmed E-utilities
\url{https://www.ncbi.nlm.nih.gov/books/NBK25501/}.
}
\details{
An API key is not mandatory but may avoid quota limitation for searches that
return a large number of results. Large results sets are obtained by
iterative querying.
}
\examples{

\dontrun{
# Initial query to be built on domain knowledge. It accepts OR, AND, NOT
# boolean operators and round brackets to group terms.
query <- '((model OR models OR modeling OR network OR networks) AND
(dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR
hospital OR "long-term-care" OR "long term care" OR "longterm care" OR
"long-term care" OR "healthcare associated") AND (infection OR resistance OR
resistant))'

# Year filter. The framework converts it to the API-specific format seamlessly.
# common logical comparators can be used, i.e. <, <=, >, >=, while dashes
# denotes inclusive date intervals. A single year restricts results to one year
# period.
year_filter <- '2010-2020'

records <- search_pubmed(query, year_filter)
}
}
