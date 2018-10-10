
# NEEDS DOCUMENTATION!
check_names <- function(list, req_names, listName = NULL){

  missing <- !req_names %in% names(list)

  if(any(missing)) stop(listName, ' missing columns ', req_names[missing])

}


# add a function that will check the coordinates of the fraction and the flds input!
#' @importFrom dplyr %>%
#' @return TBD
#' @export
harmonize_coordinates <- function(frac_coordinates, fld_coordinates){

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
      dplyr::left_join(frac$coordinates %>%
                  dplyr::rename(frac_index = column_index),
                by = c('lat', 'lon')) ->
      lat_lon_mapping


    # Modify the fraction object so that it contains the correct grid cells.
    frac <-  frac[[var]][ , lat_lon_mapping$frac_index]


    lat_lon_mapping %>%
      dplyr::select(index = flds_column_index, lat, lon) ->
      coordinates

  } else {

    frac <- frac[[var]]

  }

  frac
}




# NEED TO BE CAREFUl about the intermeidate inputs using up too much memory!
# NEED TO DOCUMENT! this needs to be better documented and cleaned up quite a bi!
# see all of the TODOs also there are some sections of code that I did not add

#' @param frac TBD
#' @param fld TBD
#' @param fld_coordinates TDB
#' @param fld_time TBD
#' @param var
#' @import dplyr
#' @import foreach
#' @return TBD
#' @export

monthly_downscaling <- function(frac, fld, fld_coordinates, fld_time, var){

  # Check the inputs
  check_names(frac, c(var, 'coordinates', 'time'), 'frac input')
  check_names(fld, var, 'fld input')

  # If the fraction and field grid cells are different, becasue of droped NAs, then update the fraction
  # file so that it contains the correct grid cells.
  frac <- harmonize_coordinates(frac$coordinates, fld_coordinates)

  # Now that we know that the grid cells in the data frames represent the same thing we can start the temporal downscaling.
  # Replicate the data frame 12 times, so that there is a copy of the grid cells for each month, add the yera names. s
  fld            <- fld[[var]]
  row.names(fld) <- fld_time
  fld_12         <- replicate(n = 12, fld, simplify = FALSE)

  # Time information
  yr_fld   <- nrow(fld)             # number of years
  month_ch <- sprintf('%02d', 1:12) # string version of month number

  # Monthly downscaling
  downscale_fun <- function(mon_num, frac, fld_12, yr_fld, fld_time){

    month_frac <- matrix(rep(frac[mon_num, ], yr_fld), nrow = yr_fld, byrow = TRUE)

    monthly <- fld_12[[mon_num]] * month_frac;

    row.names(monthly) <- paste0(fld_time, month_ch[mon_num])

    monthly
  }
  monthly_data_unordered <- foreach(mon_num =1:nrow(frac), .combine = 'rbind') %do% downscale_fun(mon_num = mon_num, frac = frac, fld_12 = fld_12,
                                                                    fld_time = fld_time, yr_fld = yr_fld)

  # Orgnaize monthly down scaled results by yearmonth
  order        <- order(as.integer(row.names(monthly_data_unordered)))
  monthly_data <- monthly_data_unordered[row.names(monthly_data_unordered)[order], ]


  # Clean up intermediate inputs to avoid R memory problems
  remove(fld, fld_12, monthly_data_unordered)


  # convert montly data to the correct unnits!
  if(var == 'tas'){

    # Convert from K to C using the conversion function
    monthly_converted <- tas_conversion(monthly_data)
    unit_value <- 'C'

  } else if (var == 'pr'){

    # Convert from kg/m2*s to mm/month
    monthly_converted <- pr_conversion(monthly_data)
    unit_value <- 'mm_month-1'

  } else {

    stop('unkown variable, need to add appropriate function')

  }


  list(data = monthly_converted, coordinates = coordinates, units = unit_value)

}
