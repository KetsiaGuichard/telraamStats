% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_state.R
\name{get_telraam_token}
\alias{get_telraam_token}
\title{Get the current authentication token for the 'Telraam' API}
\usage{
get_telraam_token()
}
\value{
Token currently used, set by \code{set_telraam_token()}
}
\description{
Get the current authentication token for the 'Telraam' API
}
\examples{
my_token <- "MyTelraamToken"
set_telraam_token(my_token)
get_telraam_token()

}
