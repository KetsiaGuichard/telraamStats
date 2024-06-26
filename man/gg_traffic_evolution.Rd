% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_evolution_traffic.R
\name{gg_traffic_evolution}
\alias{gg_traffic_evolution}
\title{Evolution of traffic and smoothed traffic.}
\usage{
gg_traffic_evolution(
  enriched_data,
  date_range = NULL,
  segments = NULL,
  modes = c("heavy", "car"),
  direction = "both",
  smoothed = TRUE,
  agg_day = TRUE
)
}
\arguments{
\item{enriched_data}{enriched data.frame containing all the data for all your sensors}

\item{date_range}{Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL (default).}

\item{segments}{Character vector. Selected road segment by its name, all if NULL (default).}

\item{modes}{Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car}

\item{direction}{Character. Direction of the traffic (lft, rgt, both). Default to both.}

\item{smoothed}{Boolean. Should the smoothed traffic be plotted ? Default: True}

\item{agg_day}{Boolean. Should the data be aggregated per day ? Default : True}
}
\value{
Graph showing the evolution of traffic (for specified parameters) during the specified period.
}
\description{
Evolution of traffic (global, per mode ou per direction), smoothed traffic
during a period.
}
\examples{
gg_traffic_evolution(traffic)
gg_traffic_evolution(traffic,
  date_range = c('2022-01-01','2022-03-01'),
  segment = 'RteVitre-06',
  mode = c('car','pedestrian'),
  direction = 'lft',
  smoothed = FALSE,
  agg_day = FALSE)
}
