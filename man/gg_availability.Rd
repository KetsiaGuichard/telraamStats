% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_quality.R
\name{gg_availability}
\alias{gg_availability}
\title{Availability and quality of sensors during a period through a heatmap.}
\usage{
gg_availability(enriched_data, date_range = NULL)
}
\arguments{
\item{enriched_data}{enriched data.frame containing all the data for all your sensors}

\item{date_range}{Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL.}
}
\value{
Graph showing availability and quality of sensors over the selected date range.
}
\description{
Higher is the uptime average, higher is the quality of data.
A null uptime means that the sensor wasn't available during this period.
}
\examples{
 gg_availability(traffic)
}
