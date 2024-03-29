% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NLP.R
\name{DTM.add_ngrams}
\alias{DTM.add_ngrams}
\title{Find non-consecutive n-grams}
\usage{
DTM.add_ngrams(DTM, min.sim = 0.5, max.terms = 10)
}
\arguments{
\item{DTM}{A Document Term Matrix.}

\item{min.sim}{The minimal cosine similarity that identifies an edge.}

\item{max.terms}{The maximum size (i.e., the number of terms) in an n-gram.}
}
\value{
The same input Document Term Matrix with extra columns for the
  n-grams.
}
\description{
Build a term-term network using a cosine similarity measure built on the term
co-presence in documents. A threshold defined in \code{min.sim} is used to
identify edges. The maximal cliques of the network represent the discovered
n-grams.
}
