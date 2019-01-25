
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
    frac <- frac[[var]][ , lat_lon_mapping$frac_index]

    } else {

    frac <- frac[[var]]

  }

  frac
}


#' Downscale annual data to monthly data.
#'
#' \code{monthly_downscaling} Use average monthly fractions of annual tas or pr ISMIP/CMIP5 data to downscale annual data to montly values.
#'
#' @param alpha Matrix[ngrid, 12] of the alpha parameters for the Dirichlet
#' distribution.  These values are included as package data.  For more details
#' on how they are calculated see data-raw.
#' @param fld_data the 2d array of ntime x ngrid of the data to be downscaled, this object should be a flattened 3-D array, where latitude
#' the most rapidly varying index for the individual time slices.
#' @param fld_coordinates a data frame of the grid cells coordinates for the field matrix.
#' @param fld_time a vector of the field time values.
#' @param var the string of the variable to process.
#' @importFrom  foreach %do% %dopar%
#' @return a list containing the monthly downsaceld 2d array and a
#' @export
monthly_downscaling <- function(alpha, fld_data, fld_coordinates, fld_time, var){

  igrid <- NULL                         # silence package checks.

  # Check the inputs
  if(!is.array(fld_data)){stop('fld_data must be an array')}
  check_names(list = alpha, req_names = c(var, 'coord', 'time'), list_name = 'frac input')

  # Re-index the grid cells so that the monthly fractions used in the downscaling and the data being downscaled
  # have the same latitude and longitude coordinate system. This is important when working with ISIMIP and CMIP files.
  # Since the ISMIP data only has data for grid cells over land.
  alpha <- reindex_grid(alpha, alpha$coord, fld_coordinates, var)
  if(nrow(alpha) != 12) stop('there must be 12 rows one for each month, in the alpha input')

  # Start temporal downscaling
  nyear   <- nrow(fld_data)        # number of years

  ## Annual values are averages, but we need totals, so multiply these values
  ## by 12.
  fld_data <- fld_data * 12

  ## Loop over grid cells; for each grid cell, generate yr_fld sets of monthly
  ## fractions, and apply them to the annual total.
  ngrid <- ncol(fld_data)
  rngstate <- .Random.seed
  monthly_data <- foreach::foreach(igrid = 1:ngrid, .combine='cbind') %dopar% {
    ## Use the same rng state for each grid cell.  This prevents us from having
    ## excessive spatial variation within a single month.
    .Random.seed <<- rngstate

    monthly_fractions <- gtools::rdirichlet(nyear, alpha[,igrid]) # matrix[nyear,12]
    annual_totals <- fld_data[,igrid]                             # vector[nyear]

    ## This next part works because multiplying two vectors of unequal length
    ## recycles the shorter vector.  Because data in matrices varies most
    ## rapidly along the rows, this has the effect of multiplying m[i,] by v[i].
    monthly_totals <- monthly_fractions * annual_totals # matrix[nyear, 12]

    ## Now we need to flatten this into a vector.  We want all the months from a
    ## single year to be together, so we need to transpose.
    as.vector(t(monthly_totals))
  }

  ## We need to add the time codes as row names.  We now have 12x as many rows
  ## as we used to, so we need to do some replicating
  year_code <- rep(fld_time, rep(12, length(fld_time))) # Each time code gets
                                        # repeated 12 times in succession
  month_code <- seq(from=0, length.out=length(year_code)) %% 12 + 1
  row.names(monthly_data) <- sprintf('%04d%02d', year_code, month_code)

  # Return the monthly data and the coordinates as a list
  list(data = monthly_data, coordinates = fld_coordinates)

}


#' Wrapper function for running downscaling from cassandra component
#'
#' Convert inputs received from the cassandra component to the format expected
#' by \code{\link{monthly_downscaling}} and pass the converted arguments.
#'
#' The conversions performed are
#' \describe{
#' \item{alpha}{Dataset is looked up by name.}
#' \item{fld_data}{The downscaling function wants a single matrix.  This
#' function accepts a list of matrices and calls the downscaling function on
#' each one sequentially.}
#' \item{coords}{The matrix of coordinate data is converted to a data
#' frame, and the index column is added.}
#' \item{time}{No conversion.}
#' \item{var}{No conversion.}
#' }
#'
#' @param alpha Name of monthly fraction dataset to use.
#' @param fld_data List of matrices of annual field data to downscale to monthly.
#' @param coords Coordinate matrix for the field data, such as the one
#' produced by \code{\link[fldgen]{coord_array}}.
#' @param time Vector of years for the input field matrices.
#' @param var String indicating the variable being downscaled.  Either 'tas' or
#' 'pr'.
#' @param ncore Number of cores to use for parallel processing of grid cell data.
#' @return List of matrices of downscaled data.  Grid cells are in columns, months
#' in rows.
#' @export
#' @keywords internal
downscaling_component_api <- function(alpha, fld_data, coords, time, var, ncore=4)
{
    alpha <- get(alpha)   # would be better to use getFromNamespace, but it doesn't seem to work.

    doParallel::registerDoParallel(cores=ncore)

    if(is.null(colnames(coords)))
        colnames(coords) <- c('lat','lon') # Won't be necessary if you use the
                                        # output of coord_array
    fld_coordinates <- as.data.frame(coords)
    fld_coordinates$column_index <- seq(nrow(fld_coordinates))

    ## return the list of downscaled fields
    lapply(fld_data,
           function(f) {
               md <- monthly_downscaling(alpha, f, fld_coordinates, time, var)
               colnames(md$data) <- NULL
               md$data
           })

}
