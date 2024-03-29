% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Reporting.R
\name{summarise_vector}
\alias{summarise_vector}
\title{Summarise a discrete vector}
\usage{
summarise_vector(vec)
}
\arguments{
\item{vec}{A vector.}
}
\value{
A string listing each unique element in the vector, its numerosity
  and percentage over the vector length (including missings).
}
\description{
Shows for each unique element its numerosity and percentage.
}
\examples{

summarise_vector(iris$Species)
# setosa: 50 (33.3\%), versicolor: 50 (33.3\%), virginica: 50 (33.3\%)

}
