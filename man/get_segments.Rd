% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_global.R
\name{get_segments}
\alias{get_segments}
\title{Get Telraam segments into a named vector}
\usage{
get_segments()
}
\value{
Named vector with names and segment IDs, NULL if there is no configuration file
}
\description{
Get Telraam segments info in yml file and transform them into a named vector
}
\examples{
create_config(create_directory = FALSE)
get_segments()
}
\keyword{internal}
