#' Convert tas from K to C
#'
#' @param input a 2d array of temperature data in degree K to convert to C
#' @keywords internal
#' @return a matrix of the monthly downscaled temperature in degrees C
#' @export

tas_conversion <- function(input){

  input - 273.15

}


#' Convert from monthly downscaled preciptation from kg/m2*s to mm/month
#'
#' @param input a 2d array of monhtly downscaled preciptation data in kg/m2*s to convert to mm/month, row.names of input must correspond to a time formatted as YYYYMM
#' @importFrom lubridate ymd interval %m+%
#' @return a matrix of monthly precipitation data in mm/month
#' @export

pr_conversion <- function(input){

  if(is.null(time)|any(is.na(time))){stop('input 2d array needs row.names')}

  # Parse out time information from the input and check the time information.
  time <- paste0(row.names(input), '01')
  test <- suppressWarnings(lubridate::ymd(time))
  if(nchar(time[[1]]) != 8){ stop('row.names of input must be YYYYMM format') }

  # Add an extra month on to the time vector to use in the time span calculation.
  extra_step <- gsub("-", "", lubridate::ymd(time[length(time)]) %m+% base::months(1))
  time_steps <- c(time, extra_step)

  # Calculate the time span within each month.
  span <- lubridate::interval(time_steps[1:(length(time_steps)-1)], time_steps[2:length(time_steps)])

  # Parse out the number of seconds from the monthly time span.
  seconds <- as.vector(as.numeric(lubridate::as.duration(span), "seconds"))

  # Format seconds per month into a matrix.
  second_matrix <- matrix(rep(seconds, each = ncol(input)), nrow = nrow(input), byrow = TRUE)

  # Multiply the kg/m2*s by the number of seconds in each month to convert to mm/month.
  input * second_matrix

}



