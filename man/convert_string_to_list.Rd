% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_import.R
\name{convert_string_to_list}
\alias{convert_string_to_list}
\title{Convert a character string into a numeric vector}
\usage{
convert_string_to_list(vector)
}
\arguments{
\item{vector}{Something in the shape "10,20,30"}
}
\value{
Numeric vector. Something in the shape c(10,20,30)
}
\description{
Convert a character string into a numeric vector
}
\examples{
convert_string_to_list("10,20,30")

}
\keyword{internal}
