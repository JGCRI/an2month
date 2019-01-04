
# Purpose: Format the monthly fractino netcdfs from L1A into a list of the time, girdcell corrdinate information, and the data frame of
# month by grid cell monthly fraction.

# TODO this script will need to be rewritten to be more flexible but due to time constraints we are only going to process one of the
# isimip rcp monthly fractions since we proved that the rcp monthly fractions are essentially the same. Will also want to look into
# how to document data....

# TODO this script uses a mapping file to replace the expected NAs in the Sahara wtih 1/12.The mapping file is hard coded.

# 0. Set Up -----------------------------------------------------------------------------------

# Set up the directories
BASE <- getwd() # should be the project location
INPUT  <- file.path(BASE, 'data-raw', 'output-L1')
OUTPUT <- file.path(BASE, 'inst', 'extdata')

# Load the libraries
library(dplyr)
library(tidyr)
library(netcdf)
library(tibble)

# Import the missing cells mapping file
NA_mapping <- read.table('C:/Users/dorh012/Documents/an2month/data-raw/mapping/missing_cells_mapping.tsv')

# 1. Import netcdfs -----------------------------------------------------------------------------------

# the pr netcdf
pr_nc_path <- list.files(INPUT, 'pr_ipsl-cm5a-lr_rcp2p6_avgFrac.nc', full.names = TRUE)
pr_nc      <- nc_open(pr_nc_path)

# the tas netcdf
tas_nc_path <- list.files(INPUT, 'tas_ipsl-cm5a-lr_rcp2p6_avgFrac.nc', full.names = TRUE)
tas_nc      <- nc_open(tas_nc_path)


# Extract the pr, tas, time, lat and lon data.
pr_data  <- ncvar_get(pr_nc, 'pr')
tas_data <- ncvar_get(tas_nc, 'tas')
time     <- ncvar_get(tas_nc, 'time')
lon      <- ncvar_get(tas_nc, 'lon')
lat      <- ncvar_get(tas_nc, 'lat')


# 2. Flatten the 3d array -----------------------------------------------------------------------------------

# This is the same method that is used by the fldgen to flatten the 3d array with lat as the most rapidly
# varying dimension.

# First save the dim of lat, lon, and time.
nlat  <- length(lat)
nlon  <- length(lon)
ntime <- length(time)

# Format the fractions
pr_frac      <- aperm(pr_data, c(3,2,1))
dim(pr_frac) <- c(ntime, nlat*nlon)
pr_frac      <- as.matrix(pr_frac)

tas_frac      <- aperm(tas_data, c(3,2,1))
dim(tas_frac) <- c(ntime, nlat*nlon)
tas_frac      <- as.matrix(tas_frac)


# 3. Format the time and corrdinates list -----------------------------------------------------------------------------

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
pr_frac[ , replace$column_index] <- 1/12


# Check to make sure that the number of gridcells in the coordinates mapping file matches the number of
# grid cells in the fraction matrix.
if(nrow(coordinates) != ncol(tas_frac)) stop('Unxpected number of rows in the coordinates mapping file')


# Add the month name to the time
month <- substr(start = 5, stop = 6, x = time)
month_name <- c("January", "February", "March", "April", "May", "June", "July", "August",
                "September", "October", "November", "December")
time_mapping <- tibble(month = month, month_name = month_name)



# 3. Save the output as package data ----------------------------------------------------------------------------

frac_ipsl_cm5a_lr <- list(tas = tas_frac, pr = pr_frac, coordinates = coordinates, time = time_mapping)

devtools::use_data(frac_ipsl_cm5a_lr, overwrite = TRUE)


# End
# Check the fractions to make sure that fractions look good
# apply(pr_frac, MARGIN = 2, sum) -> check_pr
# apply(tas_frac, MARGIN = 2, sum) -> check_tas
