# These functions are used in the conversions

#' @param input
#' @keywords internal
#' @return TBD

tas_conversion <- function(input){

  input - 273.15 # K to C


}

# TODO this function requires lubridate, will need to add documentation.

#' @param input
#' @import lubridate
#' @keywords internal
#' @return TBD
pr_conversion <- function(input){

  # Generate number of seconds for all months in data, will need to
  # step forwad beyond the last time setp in order to calculate the
  # number of seconds in each month of data. This will account for
  # leap years.

  time <- paste0(names(input), '01')
  extra_step <- gsub("-", "", ymd(time[length(time)]) %m+% months(1))
  time_steps <- c(time, extra_step)


  # Get the span of time for each month
  span <- interval(time_steps[1:(length(time_steps)-1)], time_steps[2:length(time_steps)])

  # Parse out the number of seconds from the span
  seconds <- as.vector(as.numeric(as.duration(span), "seconds"))

  # Multiply the kg/m2*s by the number of seconds in each month
  # to convert to mm/month.
  rslt <- list()
  for(i in 1:length(input)){

    rslt[[i]] <-  signif(input[[i]] * seconds[[i]], digits = 5)

  }


  # Add the names to the output
  names(rslt) <- names(input)

  rslt
}



