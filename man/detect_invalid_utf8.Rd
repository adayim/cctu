% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utf8.R
\name{detect_invalid_utf8}
\alias{detect_invalid_utf8}
\alias{remove_invalid_utf8}
\title{Functions to detect and delete non-UTF8 characters, which the XML output
will not like}
\usage{
detect_invalid_utf8(data)

remove_invalid_utf8(data)
}
\arguments{
\item{data}{a data.frame, typically the input to \code{write_table}}
}
\value{
\code{detect_invalid_utf8} returns a data.frame that gives the
column, row, and value of any datum points that contain invalid characters,
or a zero-length data frame if none are present.  \code{remove_invalid_utf8}
replaces any invalid characters with an empty string.
}
\description{
Functions to detect and delete non-UTF8 characters, which the XML output
will not like
}
\section{Functions}{
\itemize{
\item \code{remove_invalid_utf8()}: Function to delete invalid UTF8 characters

}}
\seealso{
\code{\link{write_table}}
}
