% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timing.R
\name{wait_and_window}
\alias{wait_and_window}
\title{Wait and window}
\usage{
wait_and_window(
  then,
  now = Sys.time(),
  wait = as.numeric(Sys.getenv("PHOTOTWEETR_WAIT", one_week)),
  window = c(8, 22),
  verbose = TRUE
)
}
\arguments{
\item{then}{a previous time (\code{POSIXct})}

\item{now}{the current time (default: \code{Sys.time()})}

\item{wait}{the seconds to wait between \code{then} and \code{now} (default: \code{PHOTOTWEETR_WAIT}
environment variable or one week if unset)}

\item{window}{hours between which to return \code{TRUE}}

\item{verbose}{should the wait/window \code{message} why? (default: \code{TRUE})}
}
\value{
logical
}
\description{
Compare a time from the past (\code{then}) to the current time (\code{now}, default:
\code{Sys.time()}) and see if \code{wait} seconds (default: 1 week) has passed.
additionally, only return \code{TRUE} if the current time is between the hours
given in \code{window} (if \code{window} is null, then always return)
}
