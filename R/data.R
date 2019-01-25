#' Dirichlet parameters for the ISIMIP models
#'
#' These datasets contain the the fits of the Dirichlet distribution alpha
#' parameters for temperature and precipitation for the four ISIMIP models
#' supported by this package.  These structures are suitable for passing as the
#' \code{alpha}
#' argument to \code{\link{monthly_downscaling}}.
#'
#' @format List with 4 elements
#' \describe{
#' \item{tas}{Matrix [12 x ngrid] of alpha parameters for temperature for each grid
#' cell.  Non-land grid cells will have \code{NA} values.}
#' \item{pr}{Matrix [12 x ngrid] of alpha parameters for precipitation  for each
#' grid cell.  Non-land grid cells will have \code{NA} values.}
#' \item{coord}{Data frame of grid cell coordinates.}
#' \item{time}{Data frame of month names and numbers.}
#' }
#' @name alpha
NULL

#' @rdname alpha
'alpha_ipsl_cm5a_lr'

#' @rdname alpha
'alpha_gfdl_esm2m'

#' @rdname alpha
'alpha_miroc_esm_chem'

#' @rdname alpha
'alpha_noresm1_m'

