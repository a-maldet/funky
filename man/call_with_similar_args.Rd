% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funky.R
\name{call_with_similar_args}
\alias{call_with_similar_args}
\title{Call a function with similar arguments as current function call}
\usage{
call_with_similar_args(fn, ..., skip = NULL, n = 0L)
}
\arguments{
\item{fn}{The function that should be called.}

\item{...}{Additional function parameters, which should be added to the
arguments of the current function call.}

\item{skip}{An optional character vector holding names of
arguments of the current function call which should \strong{not be used} for the
call of \code{fn}.}

\item{n}{The number ob frames to go back. The default value \code{n = 0L} means,
that the arguments of the current function call should be used.}
}
\value{
The return value of the called function given in \code{fn}.
}
\description{
This function allows can be called from within a function call
(e.g. \code{foo(x = 1, y = 2)}) in order to call another function
(e.g. \verb{baz(x = 1, y = 2, z = 3}) with using the same (or
almost the same) arguments as in the current function call
(e.g. by calling \code{call_fn_with_similar_args(baz, z = 3)} inside of \code{foo()}).
}
