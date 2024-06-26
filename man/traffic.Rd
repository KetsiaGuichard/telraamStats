% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{traffic}
\alias{traffic}
\title{Traffic data of Telraam sensors in Châteaubourg (FR)}
\format{
\subsection{\code{traffic}}{

A data frame with 16,729 rows and 22 columns:
\describe{
\item{instance_id}{Sensor number. Equals -1 if the API request was made for a road segment and not for a camera.}
\item{segment_id}{Road segment Telraam ID. Equals -1 if the API request was made for a camera and not for a road segment.}
\item{segment_name}{Additional property - Segment name specified in configuration file.}
\item{segment_fullname}{Additional property - Concatenation of the segment_id and the segment name specified in configuration.}
\item{date}{date and UTC time of the reporting interval (beginning of the interval).}
\item{day}{Additional property - Day of the reporting interval.}
\item{hour}{Additional property - Hour of the reporting interval.}
\item{weekday}{Additional property - Weekday of the reporting interval.}
\item{holiday}{Additional property - boolean, indicates whether this entry is during a French public holiday.}
\item{vacation}{Indicates whether this entry is during a French vacation period, and if true, the vacation period name.}
\item{interval}{can be "hourly" or "daily" for hourly or daily aggregate data.}
\item{uptime}{between 0 and 1, represents the portion of the reporting interval that was actively spent counting the traffic}
\item{uptime_quality}{Additional property - boolean, indicates whether this entry has an uptime greather or equal than 0.}
\item{heavy, heavy_lft, heavy_rgt}{number of heavy vehicles, total and in both directions.}
\item{car, car_lft, car_rgt}{number of cars, total and in both directions.}
\item{bike, bike_lft, bike_rgt}{number of two-wheelers, total and in both directions.}
\item{pedestrian, pedestrian_lft, pedestrian_rgt}{number of pedestrians, total and in both directions.}
\item{direction}{1, internal consistency value for Telraam.}
\item{car_speed_hist_0to70plus, car_speed_hist_0to120plus}{the estimated car speed distribution in 10 km/h bins from 0 to 70+ km/h or 120+ km/h (in percentage of the total 100\%).}
\item{timezone}{name of the Time zone where the segment can be found.}
\item{v85}{estimated car speed limit in km/h that 85\% of all cars respect}
}
}
}
\source{
\url{https://telraam-api.net/}
}
\usage{
traffic
}
\description{
Telraam sensors continously monitor a street form a citizen window.
They count heavy vehicules, cars, twok-wheelers and pedestrians, every hour.
Châteaubourg is one of the city in France with the highest density of sensors
This dataframe is a subset of sensors data in Châteaubourg for 2022.
Additional properties are not present natively in the Telraam API but are added by the package.
}
\keyword{datasets}
