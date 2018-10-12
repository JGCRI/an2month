#' Convert tas from K to C
#'
#' @param input the matrix of the monthly downscaled tas data
#' @keywords internal
#' @return a matrix of tas in degrees C

tas_conversion <- function(input){

  signif(input - 273.15, digits = 6) # K to C

}


#' Convert from kg/m2*s to mm/month
#'
#' @param input a matrix of the monthly downscaled pr data
#' @importFrom lubridate ymd interval %m+%
#' @keywords internal
#' @return a matrix of pr data in mm/month

pr_conversion <- function(input){

  # Add an extra month on to the time vector to use in the time span calculation
  time <- paste0(row.names(input), '01')
  extra_step <- gsub("-", "", lubridate::ymd(time[length(time)]) %m+% months(1))
  time_steps <- c(time, extra_step)

  # Get the span of time for each month
  span <- lubridate::interval(time_steps[1:(length(time_steps)-1)], time_steps[2:length(time_steps)])

  # Parse out the number of seconds from the span
  seconds <- as.vector(as.numeric(lubridate::as.duration(span), "seconds"))

  # Format seconds per month into a matrix
  second_matrix <- matrix(rep(seconds, each = ncol(input)), nrow = nrow(input), byrow = TRUE)

  # Multiply the kg/m2*s by the number of seconds in each month to convert to mm/month.
  signif(input * second_matrix, digits = 6)

}



