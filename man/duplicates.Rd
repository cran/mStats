% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mStats.R
\name{duplicates}
\alias{duplicates}
\title{Report, tag or drop the duplicate observations}
\usage{
duplicates(data, ..., drop = FALSE)
}
\arguments{
\item{data}{data.frame}

\item{...}{variables to find the duplicate observations}

\item{drop}{\code{TRUE} deletes all the duplicate observations.}
}
\value{
data.frame with a column \code{dup_num}, indicating the number of duplicate
observations of each group of observations
}
\description{
\code{duplicates()} generates a table showing the
duplicate \code{Observations} as one or more copies as well as
their \code{Surplus} indicating the second, third, \code{...} copy of
the first of each group of duplicates.
}
\details{
If no variable is specified in \code{...}, all variables are used
to find the duplicate observations.

If \code{drop} is set to \code{TRUE}, all occurrences of each group
of observations except the first are deleted from the
dataset.
}
\examples{

x <- duplicates(iris, Species)
x <- duplicates(iris)

}
\author{
Email: \email{dr.myominnoo@gmail.com}

Website: \url{https://myominnoo.github.io/}
}
