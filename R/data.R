#' Monthly temperature and precipitation fractions for the IPSL model
#'
#' This dataset contains the data for downscaling annual mean temperature and
#' precipitation fractions to monthly values for the IPSL-CM5A-LR model from the
#' ISIMIP experiment.  This structure is suitable for passing as the \code{frac}
#' argument to \code{\link{monthly_downscaling}}.
#'
#' @format List with 4 elements
#' \describe{
#' \item{tas}{Matrix [12 x ngrid] of monthly temperature fractions for each grid
#' cell.}
#' \item{pr}{Matrix [12 x ngrid] of monthly precipitation fractions for each
#' grid cell.}
#' \item{coordinates}{Data frame of grid cell coordinates.}
#' \item{time}{Data frame of month names and numbers.}
#' }
'frac_ipsl_cm5a_lr'

