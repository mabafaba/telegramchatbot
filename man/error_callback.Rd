% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error_handling.R
\name{error_callback}
\alias{error_callback}
\title{default error handler
Catches all errors to ensure a live bot does not crash for all users when issues occur}
\usage{
error_callback(bot, error)
}
\arguments{
\item{bot}{telegram.bot bot object}

\item{error}{The Error}
}
\value{
corresponding warning
}
\description{
default error handler
Catches all errors to ensure a live bot does not crash for all users when issues occur
}
