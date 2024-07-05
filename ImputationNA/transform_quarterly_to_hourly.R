#' Convert quarterly traffic data to hourly data
#'
#' This function aggregates quarterly traffic data to hourly intervals, calculating sums and averages for specific traffic metrics.
#'
#' @param sensors A data frame containing quarterly traffic sensor data.
#'
#' @return A data frame with hourly aggregated traffic data.
#'
#' @details
#' The function accepts a data frame with specific traffic-related columns.
#' Required columns are: date, segment_id, day, hour.
#' Other columns that will be summed if present: car, heavy, bike, pedestrian,
#' car_lft, car_rgt, heavy_lft, heavy_rgt, bike_lft, bike_rgt, pedestrian_lft, pedestrian_rgt.
#' The 'uptime' column will be averaged if present.
#'
#' @importFrom dplyr mutate select filter left_join group_by summarise across everything
#' @importFrom lubridate minute
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hourly_data <- transform_quarterly_to_hourly(quarterly_sensors_data)
#' }
transform_quarterly_to_hourly <- function(sensors) {
  # Check for required columns
  required_cols <- c("date", "segment_id", "day", "hour")
  if (!all(required_cols %in% names(sensors))) {
    missing_cols <- setdiff(required_cols, names(sensors))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }


  # Define columns to be summed
  sum_cols <- c("car", "heavy", "bike", "pedestrian",
                "car_lft", "car_rgt", "heavy_lft", "heavy_rgt",
                "bike_lft", "bike_rgt", "pedestrian_lft", "pedestrian_rgt")

  # Identify which of these columns are present in the data
  present_sum_cols <- intersect(sum_cols, names(sensors))

  # Start of the main function logic
  hourly_data <- sensors %>%
    mutate(date = as.POSIXct(.data$date),
           interval = "hourly") %>%
    filter(minute(.data$date) == 0) %>%
    select(date, segment_id, day, hour, interval)

  # Perform aggregation
  aggregated_data <- sensors %>%
    group_by(.data$segment_id, .data$day, .data$hour) %>%
    summarise(
      uptime = if ("uptime" %in% names(.)) mean(.data$uptime, na.rm = TRUE) else NULL,
      across(all_of(present_sum_cols), \(x) sum(x, na.rm = TRUE)),
      vehicle = if (all(c("car", "heavy") %in% names(.)))
        sum(.data$car + .data$heavy, na.rm = TRUE)
      else NULL,
      .groups = "drop"
    )

  # Join the aggregated data with the hourly data
  result <- left_join(hourly_data, aggregated_data, by = c("segment_id", "day", "hour"))

  return(result)
}
