% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_global.R
\name{set_global_vars}
\alias{set_global_vars}
\title{Function to set up the global variables for public holidays and vacations, with the default
being the french dates from a governmental API.}
\usage{
set_global_vars(vacations = NULL, public_holidays = NULL)
}
\arguments{
\item{vacations}{data frame containing the vacation dates}

\item{public_holidays}{data frame containing the public holidays dates}
}
\value{
Don't return anything, set up the global variables for public holidays and vacations.
}
\description{
Function to set up the global variables for public holidays and vacations, with the default
being the french dates from a governmental API.
}
\examples{
pkg.globals <- new.env(parent = emptyenv())
set_global_vars()
print(pkg.globals$vacations)
}
\keyword{internal}
