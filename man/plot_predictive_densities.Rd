% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Reporting.R
\name{plot_predictive_densities}
\alias{plot_predictive_densities}
\title{Plot posterior predictive distributions generated by the classification model}
\usage{
plot_predictive_densities(
  session_name,
  sessions_folder = getOption("baysren.sessions_folder")
)
}
\arguments{
\item{session_name}{A session identifiers corresponding to folders into the
\code{sessions_folder} folder.}

\item{sessions_folder}{Where to find the \code{sessions} folders.}
}
\value{
A \code{ggplot2} object.
}
\description{
For each Classification/Review iteration, the function plots the mixture of
the posterior predictive distributions (PPD) of a positive match as predicted
by the classification model for the positive, negative, unknown labelled
records in the Annotation data sets.
}
