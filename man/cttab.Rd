% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cttab.R
\name{cttab}
\alias{cttab}
\title{Generate an table of descriptive statistics.}
\usage{
cttab(
  vars,
  data,
  group = NULL,
  row_split = NULL,
  total = TRUE,
  select = NULL,
  add_missing = TRUE,
  add_obs = TRUE,
  dlu = cctu_env$dlu,
  subjid_string = "subjid"
)
}
\arguments{
\item{vars}{Variables to be used for summary table.}

\item{data}{A \code{data.frame} from which the variables in \code{vars}
should be taken.}

\item{group}{Name of the grouping variable.}

\item{row_split}{Variable that used for splitting table rows, rows will be
splited using this variable. Useful for repeated measures.}

\item{total}{If a "Total" column will be created (default). Specify
\code{FALSE} to omit the column.}

\item{select}{a named vector with as many components as row-variables. Every
element of `select` will be used to select the individuals to be analyzed
 for every row-variable. Name of the vector corresponds to the row variable,
 element is the selection.}

\item{add_missing}{If missing number and missing percentage will be
reported in the summary table, default is `TRUE`.}

\item{add_obs}{Add an observation row (default).}

\item{dlu}{A data.frame of DLU file.}

\item{subjid_string}{A character naming the column used to identify subject.}
}
\value{
A matrix with `cttab` class.
}
\description{
This is a wrapper function of \code{stat_tab}, allowing for groupped variables,
split statistics table by `row_split` variable.
}