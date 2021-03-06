% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funky.R
\name{get_call_args}
\alias{get_call_args}
\title{List all passed in function arguments}
\usage{
get_call_args(n = 0L)
}
\arguments{
\item{n}{The number ob frames to go back. The default value \code{n = 0L} means,
that the given arguments of current function should be returned.}
}
\value{
A named list holding the arguments of the function call (including
non-changed default values)
}
\description{
This functions returns a named list holding the argument values of the
current function call.
This includes:
\itemize{
\item arguments assigned by name (e.g. \code{foo(x = 1)})
\item arguments assigned by position (e.g. \code{foo(1)})
\item arguments for which the default value was overwritten (e.g. \code{foo(x = 1)} with \verb{foo <- function(x = 99)})
\item arguments for which the default value was used (e.g. \code{foo()} with \verb{foo <- function(x = 99)})
\item named and unnamed arguments inside of a three dots ellipsis
}
}
