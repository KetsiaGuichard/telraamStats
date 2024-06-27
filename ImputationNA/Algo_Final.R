#' Impute missing values for vehicle counts using a rolling mean
#'
#' This function imputes missing values for vehicle counts (car + heavy) using a rolling mean on an hourly basis.
#'
#' @param data A data frame containing the columns, 'segment_name', 'weekday', 'hour', 'car', and 'heavy'.
#' @param sensors_name A character or a vector specifying the names of the sensors to be imputed. If NULL, all sensors will be included.
#' @param window Integer specifying the window size for the rolling mean. Default is 5.
#' @return A data frame with imputed values added as 'vehicle' and an 'imputed' column indicating the imputation status.
#' @export

#' @Importfrom yaml ...

#' @examples
#' traffic_NA <- stop_sensor(traffic, successive_day = 0)
#' traffic_imputed <- impute_NA_vehicle(traffic_NA, sensors_name = "RteVitre-06", window = 5)
#' table(traffic_imputed$imputed)


impute_NA_vehicle <- function(data, sensors_name = NULL ,window = 8) {
  # Load required libraries
  library(dplyr)
  library(zoo)


  # Filter data by sensor name if provided
  if(!is.null(sensors_name)){
    data <-  data %>% filter(segment_name %in% sensors_name)
  }

  # Create the 'y' column for the sum of 'car' and 'heavy', and group data by segment, weekday, and hour

  if (!"vehicle" %in% colnames(data)) {
    data <- data %>% mutate(y = car + heavy)
  }
  else {
    data <- data %>% rename(y = vehicle)
  }


  data <- data  %>%
    group_by(segment_name,weekday, hour) %>%

    # Apply rolling mean using zoo::rollapply and create the 'vehicle' column
    mutate(vehicle = rollapply(y, width = window, FUN = function(x) mean(x, na.rm = TRUE), fill = NA,partial=T, align = "center"),

           # Create the 'imputed' column to indicate the imputation status
           imputed = case_when(
                               is.na(y) & !is.na(vehicle) ~ "imputed",
                               is.na(y) & is.na(vehicle) ~ "not imputed",
                               !is.na(y) ~ "original"
                              ),
           # Ensure that original values are preserved
           vehicle = ifelse(imputed == "original", y, vehicle)) %>%

    ungroup() %>%  select(-y)

  # Return the data frame with imputed values
  return(data)
}

