% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Record_management.R
\name{import_data}
\alias{import_data}
\title{Read a data file/object}
\usage{
import_data(input, ...)
}
\arguments{
\item{input}{A file path to an Excel/CSV/RDS file or a data frame object.}

\item{...}{Additional arguments to pass to
\code{\link[readxl:read_excel]{readxl::read_excel}},
\code{\link[readr:read_csv]{readr::read_csv}}, or
\code{\link[readr:read_rds]{readr::read_rds}}, depending on the file type.}
}
\value{
A data frame object.
}
\description{
General wrapper that imports CSV/Excel/RDS files if a file path is given in
\code{input}. If \code{input} is a data frame, it will just be passed
through. An error is raised in the case of unrecognized objects/files.
}
\examples{

\dontrun{
# These all work:

data <- import_data('data.csv')

data <- import_data('data.xlsx', sheet = 2)

data <- import_data('data.rds')

data <- import_data(data)
}
}
