% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tr_runs.R
\name{tr_runs}
\alias{tr_runs}
\title{Nest employees, add transaction runs, and unnest}
\usage{
tr_runs(.data, holidates)
}
\arguments{
\item{.data}{A data frame.}

\item{holidates}{A data frame with a \code{date} column for holiday dates.}
}
\value{
A tibble.
}
\description{
\code{tr_runs()} nests by employee, year-half, and code to calculate
transaction runs (id's for transactions that continue across
working days).
}
