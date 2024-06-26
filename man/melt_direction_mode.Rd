% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_global.R
\name{melt_direction_mode}
\alias{melt_direction_mode}
\title{Melt dataframe to obtain one row per hour/segment/transportation mode/direction
This format makes graphs with ggplot and filtering easier.}
\usage{
melt_direction_mode(data)
}
\arguments{
\item{data}{Traffic Data Frame}
}
\value{
DataFrame with one row per hour/segment/transportation mode/direction
}
\description{
Melt dataframe to obtain one row per hour/segment/transportation mode/direction
This format makes graphs with ggplot and filtering easier.
}
\examples{
melt_direction_mode(traffic[0:2,])
}
\keyword{internal}
