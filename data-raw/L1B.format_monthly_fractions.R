
# Purpose: Format the monthly fraction netcdfs from L1A into an array[nyear, nmonth, ngrid].This is
# set up to run on pic. Users will want to change the dirs defined in the section 0.

# 0. Set Up -----------------------------------------------------------------------------------

# Set up the directories
BASE   <- here::here() # should be the project location
INPUT  <- file.path(BASE, 'data-raw', 'output-L1')
OUTPUT <- file.path(BASE, 'data-raw', 'output-L2'); dir.create(OUTPUT)

# Load the libraries
library(dplyr)
library(tidyr)
library(tibble)
library(ncdf4)


# 1. Mapping Information --------------------------------------------------------------

# Find the netcdfs files on the INPUT
netcdfs <- list.files(INPUT, pattern = '.nc', full.names = TRUE)
netcdfs <- netcdfs[!grepl(x = netcdfs, pattern = 'piControl')]

# Import the missing cells mapping file
NA_mapping <- read.table(file.path(BASE, 'data-raw', 'mapping', 'missing_cells_mapping.tsv'))

# 2. Define functions ------------------------------------------------------------

# This function imports netcdfs and formats the data as an array [year, month, gridcells]
# Args:
#     path - the input path
#     var - the name of the variable
#     output_dir - the name dir to save the formated data
format_data <- function(input_path, var, output_dir){

  # Import the nc
  nc   <- nc_open(input_path)

  # Extract data from  the nc
  if(var == 'tas'){
    var <- 'tasAdjust'
  } else {
    var <- 'prAdjust'
  }

  data <- ncvar_get(nc, var)
  time <- ncvar_get(nc, 'time')
  lon  <- ncvar_get(nc, 'lon')
  lat  <- ncvar_get(nc, 'lat')

  # Extract the dim
  nlat  <- length(lat)
  nlon  <- length(lon)
  ntime <- length(time)
  ngrid <- nlat * nlon

  # Calculate the number of years in the data set
  nyears <- ntime / 12

  # Transpose the array
  data_t <- aperm(data, c(3, 2, 1))

  # Flatten the matrix by the grid cells
  dim(data_t) <- c(ntime, ngrid)


  # Only for the pr in the Sharah desert replace the monthly fractions with 1/12 to avoid
  # problems latter on. This problem does not occur with temp.
  if(var == 'prAdjust'){

    # Format the grid cell coordinat data frame.
    tibble(lat = lat) %>%
      mutate(join = 1) ->
      lat_tibble

    tibble::tibble(lon = lon) %>%
      dplyr::mutate(join = 1) %>%
      dplyr::left_join(lat_tibble, by = "join") %>%
      dplyr::mutate(column_index = 1:(nlat * nlon)) %>%
      dplyr::select(column_index, lat, lon) ->
      coordinates

    # Use the coordinates and the NA cells mapping file to repalce the NAs in the pr_frac with 1/12.
    NA_mapping %>%
      select(lat, lon) %>%
      mutate(keep = 1) %>%
      full_join(coordinates,  by = c("lat", "lon")) %>%
      na.omit ->
      replace

    # Replace NAs with 1/12
    data_t[ , replace$column_index] <- 1/12

  }

  # Format the 2d matrix by year, 12, gridcell
  dim(data_t) <- c(nyears, 12, ngrid)

  # Add names to the array
  dimnames(data_t)[[1]] <- unique(substr(time, 1, 4))
  dimnames(data_t)[[2]] <- c("January", "February", "March", "April", "May", "June", "July", "August",
                             "September", "October", "November", "December")

  # Save the array as an rds
  saveRDS(data_t, file = file.path(output_dir, gsub(basename(input_path), pattern = '.nc', replacement = '.rds')))

}




# 3. Format the data ---------------------------------------------------------------------------
tibble(path = netcdfs,
       file = basename(path)) %>%
  tidyr::separate(file,
                  into = c('var', 'model', 'exp', 'frac'),
                  remove = FALSE,
                  sep = '_') %>%
  split(.$path) %>%
  lapply(function(input){format_data(input_path = input$path, var = input$var, output_dir = OUTPUT)})


