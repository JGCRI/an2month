# These functions are used in the conversions

#' @param input
#' @keywords internal
#' @return TBD

tas_conversion <- function(input){

  signif(input - 273.15, digits = 6) # K to C


}

# TODO this function requires lubridate, will need to add documentation.

#' @param input
#' @import lubridate
#' @import foreach
#' @keywords internal
#' @return TBD
pr_conversion <- function(input){

  # Generate number of seconds for all months in data, will need to
  # step forwad beyond the last time setp in order to calculate the
  # number of seconds in each month of data. This will account for
  # leap years.

  time <- paste0(row.names(input), '01')
  extra_step <- gsub("-", "", ymd(time[length(time)]) %m+% months(1))
  time_steps <- c(time, extra_step)


  # Get the span of time for each month
  span <- interval(time_steps[1:(length(time_steps)-1)], time_steps[2:length(time_steps)])

  # Parse out the number of seconds from the span
  seconds <- as.vector(as.numeric(as.duration(span), "seconds"))

  # TODO, convert the seconds into a matrix and then mulitply!
  # Multiply the kg/m2*s by the number of seconds in each month to convert to mm/month.
  rslt <- foreach(i = 1:length(seconds), .combine = 'rbind') %do% (signif(input[i, ] * seconds[i], digits = 6))

  rslt
}



