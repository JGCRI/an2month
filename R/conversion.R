#' Convert tas from K to C
#'
#' @param input a 2d array of temperature data in degree K to convert to C
#' @keywords internal
#' @return a matrix of the monthly downscaled temperature in degrees C
#' @export

tas_conversion <- function(input){

  input - 273.15

}


#' Convert monthly downscaled preciptation from kg/m2/s to mm/month
#'
#' A kg/m2 of water is approximately one millimeter of depth (because at standard temperature
#' 1 cc of water masses 1 gram).  So, to convert to depth per unit time, you just multiply
#' by the number of seconds in the time unit you want.
#'
#' The difference between the regular and simplified version is that the simplified version
#' doesn't attempt to correct for the fact that months are not all the same length; it just
#' treats every month as 1/12 of a tropical year.
#'
#' @param input a 2d array of monhtly downscaled preciptation data in kg/m2/s to convert to mm/month, row.names of input must correspond to a time formatted as YYYYMM
#' @importFrom lubridate %m+%
#' @return a matrix of monthly precipitation data in mm/month
#' @export
pr_conversion <- function(input){


  # Parse out time information from the input and check the time information.
  time <- paste0(row.names(input), '01')
  if(is.null(time) || any(is.na(time))){stop('input 2d array needs row.names')}
  time <- suppressWarnings(lubridate::ymd(time))
  if(any(is.na(time))) {
      stop('row.names of input must be YYYYMM format')
  }


  # Add an extra month on to the time vector to use in the time span calculation.
  extra_step <- time[length(time)] %m+% base::months(1)
  ##extra_step <- gsub("-", "", lubridate::ymd(time[length(time)]) %m+% base::months(1))
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

#' @describeIn pr_conversion Simplified conversion of monthly downscaled preciptation from kg/m2/s to mm/month
#'
#' @export
pr_conversion_simple <- function(input)
{
  tropyr_sec <- 365.24219 * 86400   # in seconds
  month_sec <- tropyr_sec / 12.0

  input * month_sec
}
