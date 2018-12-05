
#' Check an object for required names
#'
#' @param list an object, such as list or data frame that needs certain names
#' @param req_name a vector of the required names in the list
#' @param list_name an optional string that will be incorporated into the error message, default is NULL
#' @return an error message if the list is missing a required name
#' @keywords internal
check_names <- function(list, req_names, list_name = NULL){

  missing <- !req_names %in% names(list)

  if(any(missing)) stop(list_name, ' missing ', req_names[missing])

}


#' Re-index the grid so that coordinates match latitude and longitude
#'
#' \code{reindex_grid}  returns the fraction matrix to use in \code{monthly_downscaling} during the monthly downscaling process.
#'
#' This function ensures that that the monthly fractions used in the downscaling and the data being downscaled have the same
#' latitude and longitude coordinate system. This is important when working with ISIMIP and CMIP5 files since ISMIP has NA
#' values that may be removed during an earlier step which would change the grid cell indexing of the data.
#'
#' @param frac the downscaling fraction list, internal pacakge data
#' @param frac_coordinates the data frame of the fraction grid cell coordinates should be available from frac
#' @param fld_coordinates the data frame of the field grid cell coordinates
#' @param var the name of the variable to process, a string of tas or pr
#' @importFrom dplyr %>% rename left_join
#' @return the fraction matrix to use in monthly downscaling arranged so that the fraction grid cells match the order of the field grid cells
#' @keywords internal
reindex_grid <- function(frac, frac_coordinates, fld_coordinates, var){

  # Silence package checks
  '%>%' <- 'column_index' <- NULL

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

  # If the fraction file has larger dimensions then it could be caused by the NAs, use the
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


#' Downscale annual data to monthly data.
#'
#' \code{monthly_downscaling} Use average monthly fractions of annual tas or pr ISMIP/CMIP5 data to downscale annual data to montly values.
#'
#' @param frac the matrix of the monthly fractions to use in downscaling, package data for more details on how calculated see data-raw
#' @param fld_data the 2d array of ntime x ngrid of the data to be downscaled, this object should be a flattened 3-D array, where latitude
#' the most rapidly varying index for the individual time slices.
#' @param fld_coordinates a data frame of the grid cells coordinates for the field matrix.
#' @param fld_time a vector of the field time values.
#' @param var the string of the variable to process.
#' @importFrom  foreach %do%
#' @return a list containing the monthly downsaceld 2d array and a
#' @export
monthly_downscaling <- function(frac, fld_data, fld_coordinates, fld_time, var){

  # Silence package checks
  '%do%' <- 'mon_num' <- NULL

  # Check the inputs
  if(!is.array(fld_data)){stop('fld_data must be an array')}
  check_names(list = frac, req_names = c(var, 'coordinates', 'time'), list_name = 'frac input')

  # Re-index the grid cells so that the monthly fractions used in the downscaling and the data being downscaled
  # have the same latitude and longitude coordinate system. This is important when working with ISIMIP and CMIP files.
  # Since the ISMIP data only
  frac <- reindex_grid(frac, frac$coordinates, fld_coordinates, var)
  if(nrow(frac) != 12) stop('there must be 12 rows one for each month, in the frac input')

  # Start temporal downscaling
  # First replicate the data frame 12 times, so that there is a copy of the the annual grid cells for each month.
  # Latter on we will multiply each of copy by the monthly fractions.
  row.names(fld_data) <- fld_time
  fld_12         <- replicate(n = 12, fld_data, simplify = FALSE)

  # Copy time information
  yr_fld   <- nrow(fld_data)        # number of years
  month_ch <- sprintf('%02d', 1:12) # string version of month number

  # Multiply each copy of the annual data by a single monthly fraction
  monthly_data_unordered <- foreach::foreach(mon_num = 1:nrow(frac), .combine = 'rbind') %do% {

    month_frac <- matrix(rep(frac[mon_num, ], yr_fld), nrow = yr_fld, byrow = TRUE)     # Copy the monthly fraction so that is matches the annual grid dimensions
    monthly    <- fld_12[[mon_num]] * month_frac                                        # Multiply annual by monthly
    row.names(monthly) <- paste0(fld_time, month_ch[mon_num])                           # Name rows and return
    monthly

    }

  # Organize monthly down scaled results by yearmonth
  order        <- order(as.integer(row.names(monthly_data_unordered)))
  monthly_data <- monthly_data_unordered[row.names(monthly_data_unordered)[order], ]

  # Return the monthly data and the coordinates as a list
  list(data = monthly_data, coordinates = fld_coordinates)

}
