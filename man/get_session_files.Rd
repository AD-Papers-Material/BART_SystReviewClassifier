% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Record_management.R
\name{get_session_files}
\alias{get_session_files}
\title{Retrieve the path of the resources linked to a session.}
\usage{
get_session_files(
  session_name,
  sessions_folder = getOption("baysren.sessions_folder"),
  which = c("Records", "Annotations", "DTM", "Samples", "Results")
)
}
\arguments{
\item{session_name}{The name of the session.}

\item{sessions_folder}{The folder in which all sessions are stored. It can be
initialized with the \code{baysren.sessions_folder} option.}

\item{which}{Which resource is required. The default is all of them.}
}
\value{
A list of vectors of file paths.
}
\description{
A helper to retrieve the paths to the original annotation data, the updated
files after each classification iteration, the Document Term Matrix, the
results summaries and the Bayesian posterior samples.
}
