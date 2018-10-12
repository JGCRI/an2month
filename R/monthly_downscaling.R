
#' Check an object for required names
#'
#' @param list an object, such as list or data frame that needs certain names
#' @param req_name a vector of the expected/required names in the list
#' @param listName an optional string that will be incorperated into the error message, default is NULL
#' @return an error message if the list is missing a required name
check_names <- function(list, req_names, listName = NULL){

  missing <- !req_names %in% names(list)

  if(any(missing)) stop(listName, ' missing ', req_names[missing])

}


#' Harmonize the fraction and field grid cells by coordinates
#'
#' @param frac the downscaling fraction list, should be avaiable as package data
#' @param frac_coordinates the data frame of the fraction grid cell coordinates should be avaiable from frac
#' @param fld_coordinates the data frame of the field grid cell coordinates
#' @param var the variable, tas or pr, to process
#' @importFrom dplyr %>% rename left_join
#' @return the fraction matrix to use in monthly downscaling arranged so that the fraction grid cells match the order of the field grid cells
harmonize_coordinates <- function(frac, frac_coordinates, fld_coordinates, var){

  # Check inputs
  stopifnot(is.data.frame(frac_coordinates))
  stopifnot(is.data.frame(fld_coordinates))

  frac_dim <- dim(frac_coordinates)
  fld_dim  <- dim(fld_coordinates)

  # If the dimensions of the field file is greater than the fraction file it means that the
  # fraction file resolution is less than the field
  if(any(frac_dim < fld_dim)) stop('Resolution of the monthly fraction coordinates is insufficient')
  if(any(!fld_coordinates$lat %in% frac_coordinates$lat)) stop('fraction file is missing lat values')
  if(any(!fld_coordinates$lon %in% frac_coordinates$lon)) stop('fraction file is missing lon values')

  # If the fraction file has larger dimensions then it could be casued by the NAs, use the
  # fld coordinate information to subset the fraction file.
  if(any(dim(frac_coordinates) > dim(fld_coordinates))){

    fld_coordinates %>%
      dplyr::rename(flds_column_index = column_index) %>%
      dplyr::left_join(frac_coordinates %>%
                  dplyr::rename(frac_index = column_index),
                by = c('lat', 'lon')) ->
      lat_lon_mapping


    # Modify the fraction object so that it contains the correct grid cells.
    frac <-  frac[[var]][ , lat_lon_mapping$frac_index]

    } else {

    frac <- frac[[var]]

  }

  frac
}


#' Downscale annual gridded data to monthly gridded data and convert units
#'
#' @param frac the matrix of the monthly fractions to use in downscaling, created by \code{harmonize_coordinates}
#' @param fld the full grid data to downscale
#' @param fld_coordinates the grid cell coordinates for the field matrix
#' @param fld_time a vector of the field time values
#' @param var the string of the variable to process
#' @import dplyr
#' @import foreach
#' @return a list containing a 2d data array, a data frame of grid cell coordinates, and a unit string
#' @export

monthly_downscaling <- function(frac, fld, fld_coordinates, fld_time, var){

  # Check the inputs
  check_names(frac, c(var, 'coordinates', 'time'), 'frac input')
  check_names(fld, var, 'fld input')

  #Harmonize the fraction and field grid cells by lat and lon coordinates. This is an important step because
  # NA grid cells may have been discarded when generating the fields.
  frac <- harmonize_coordinates(frac, frac$coordinates, fld_coordinates, var)
  if(nrow(frac) != 12) stop('there must be 12 rows, months of data, in the frac input')

  # Start temporal downscaling, first replicate the data frame 12 times,
  # so that there is a copy of the the annual grid cells for each month. Latter
  # on we will mulitply each of copy by the monthly fractions.
  fld            <- fld[[var]]
  row.names(fld) <- fld_time
  fld_12         <- replicate(n = 12, fld, simplify = FALSE)

  # Copy time information
  yr_fld   <- nrow(fld)             # number of years
  month_ch <- sprintf('%02d', 1:12) # string version of month number

  # Multiply each copy of the annual data by a single monthly fraction
  monthly_data_unordered <- foreach(mon_num = 1:nrow(frac), .combine = 'rbind') %do% {

    month_frac <- matrix(rep(frac[mon_num, ], yr_fld), nrow = yr_fld, byrow = TRUE)     # Copy the monthly fraction so that is matches the annual grid dimensions
    monthly    <- fld_12[[mon_num]] * month_frac                                        # Multiply annual by monthly
    row.names(monthly) <- paste0(fld_time, month_ch[mon_num])                           # Name rows and return
    monthly

    }

  # Orgnaize monthly down scaled results by yearmonth
  order        <- order(as.integer(row.names(monthly_data_unordered)))
  monthly_data <- monthly_data_unordered[row.names(monthly_data_unordered)[order], ]

  # Clean up intermediate inputs to avoid R memory problems
  remove(fld, fld_12, monthly_data_unordered)

  # Convert units
  if(var == 'tas'){

    # Convert from K to C using the conversion function
    monthly_converted <- tas_conversion(monthly_data)
    unit_value <- 'C'

  } else if (var == 'pr'){

    # Convert from kg/m2*s to mm/month
    monthly_converted <- pr_conversion(monthly_data)
    unit_value <- 'mm_month-1'

  }

  list(data = monthly_converted, coordinates = fld_coordinates, units = unit_value)

}
