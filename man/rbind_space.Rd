% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbind_space.R
\name{rbind_space}
\alias{rbind_space}
\title{Function to bind tables/matrices with a blank row between them}
\usage{
rbind_space(x, y, check.names = FALSE)
}
\arguments{
\item{x}{a data.frame or array, possibly of character elements only}

\item{y}{a data.frame or array, with the same number of columns as \code{x}}

\item{check.names}{logical. If FALSE the names are preserved from \code{x},
but if TRUE they are converted to syntactically valid variable names,
which may not be desired for a table in a report.}
}
\value{
a data.frame or array that is \code{x} stacked vertically above
\code{y} with a row of blank values in-between. Useful for formatting
}
\description{
Function to bind tables/matrices with a blank row between them
}
\details{
based largely on the \code{\link{rbind}} function.
Use in conjunction with \code{\link{Reduce}} to join up more than two tables.
}
