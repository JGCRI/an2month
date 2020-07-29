
# Purpose: This script finds and identifies the original isimip netcdfs files to process. As of 9/11/2018 we
# only have access to daily isimip netcdfs so this script will average the daily values to get monthly netcdfs.
# This script returns a csv file of the netcdfs to process in the subsequent data-raw scripts. See
# section 0 for set up and section 1 for the user decsions regarding the isimip files to search for.
#
# Right now it is set up to run on pic but can run wherever the project, input, and CDO are avaiable.

# 0. Set Up -----------------------------------------------------------------------------

showMessages <- TRUE # a T/F to supress or show script messages
if(showMessages) message('0. Set Up')

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

BASE         <- "/pic/projects/GCAM/Dorheim/an2month"     # The project location on pic
OUTPUT_DIR   <- file.path(BASE, "data-raw", "output-L0", "isimip")  # Define the output directory
CDO_DIR      <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"        # Define the cdo directory
ISIMIP_DIR   <- "/pic/projects/GCAM/ISIMIP-inputs-2b/"              # The location of the isimip files on pic

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# 1. Find the isimip files to process -----------------------------------------------------------------------------------------------------
# Create the serach netcdf search pattern and then serach the isimip directory.
if(showMessages) message('2. Find the isimip files to process')

# Search for the files, we want to process all of the daily pr and tas data.
# For all the models and all of the experiments.
tas_file_list <- list.files(ISIMIP_DIR, pattern = 'tas_mon', full.names = TRUE, recursive = TRUE)
pr_file_list <- list.files(ISIMIP_DIR, pattern = 'pr_mon', full.names = TRUE, recursive = TRUE)
file_list <- append(tas_file_list, pr_file_list)
if(length(file_list) < 1) stop('Could not find any isimip files files matching ', isimip_search_pattern)


# Parse out the isimip meta data information from the file name, this will be use to make a table to
# help illustrate what we have and identify missing netcdf files.
tibble(path = file_list) %>%
  mutate(filename = basename(path)) %>%
  separate(filename, into = c("variable", "resolution", "model", "experiment", "ensemble", "B", "C", "D"), sep = "_", remove = FALSE) %>%
  mutate(date = gsub(pattern = '.nc', replacement = '', x = D)) %>%
  select(path, variable, model, experiment, ensemble, date) %>%
  separate(date, into = c("startYr", "endYr"), sep = "-", remove = TRUE) %>%
  mutate(endYr = gsub(".nc4", "", endYr)) ->
  isimip_file_info

# 2. Check coverage  ----------------------------------------------------------------------
# We need to make sure that we have pr and tas files for each model / experiment / time period.
isimip_file_info %>%
  select(model, experiment, variable, startYr, endYr) %>%
  distinct %>%
  mutate(exsists = TRUE) %>%
  spread(variable, exsists) %>%
  gather(variable, exsist, -model, -experiment, -startYr, -endYr) ->
  check_variable

check_variable %>%
  filter(!exsist) %>%
  mutate(keep = FALSE) ->
  missing_variable

if(nrow(missing_variable)){

  # I am 99% sure that the files all match up, for now just throw an error message if it looks like
  # there might be incomplete netcdf coverage. May need to add code here latter.

  stop('Missing some variable need to add code to section 3 to
       remove incomplete netcdfs from further processing.')

}


# 3. Process to monthly  -------------------------------------------------------------------
# Becasue we only have access to the daily values we are going to need to average to the
# monthly value intorder to the the monhtly fraction in data-raw step 1.
if(showMessages) message('4. Process to monthly')

# monthly_func
# Is the function that calculates the monthly values from the daily netcdf files. This
# function requires the a path to a daily nc file, the path to the cdo, and a directory
# location where to save the netcdfs. There is an option showMessages T/F argument
# that can be used to supress function messages. The monthly netcdf will be saved at
# output_dir and the function will return the path/new.nc

monthly_func <- function(input_nc, cdo_dir, output_dir, showMessages = FALSE){

  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed
  stopifnot(file.exists(cdo_dir))
  stopifnot(dir.exists(output_dir))

  base_name <- paste0('monthly_', basename(input_nc))
  out_nc    <- file.path(output_dir, base_name)

  # Get the mean and the convert to absolute time, when we start working on the full version
  # of this script we are going to need to update this section so that the converting to absolut
  # time happens at the L1 raw data processing script!
  if(showMessages) message("Calculate the monthly average ", input_nc)
  system2(cdo_dir, args = c("monmean", input_nc, out_nc), stdout = TRUE, stderr = TRUE)

  out_nc

}

# Create a directory to save the monthly isimip netcdfs in.
inter_dir <- file.path(OUTPUT_DIR, 'monthly'); dir.create(inter_dir)

ncs <- lapply(isimip_file_info$path, FUN = monthly_func, cdo_dir = CDO_DIR, output_dir = inter_dir,
              showMessages = showMessages)

# 4. Save nc information  -------------------------------------------------------------------
# Create a tibble to save as a csv file that contains the paths and some additional information
# about the nc files to pass on to level 1 to calculate the monthly fraction data.
if(showMessages) message('4. Save nc information')

ncs <- list.files(file.path(OUTPUT_DIR, 'monthly'), full.names = TRUE)


tibble(file = list.files(file.path(OUTPUT_DIR, 'monthly'), full.names = TRUE)) %>%
  mutate(filename = basename(file)) %>%
  separate(filename, into = c("resolution", "variable", "t", "model", "experiment", "ensemble", "B", "C", "D"), sep = "_", remove = FALSE) %>%
  mutate(date = gsub(pattern = '.nc', replacement = '', x = D)) %>%
  select(path = file, variable, model, experiment, date) ->
  to_process

write.csv(x = to_process, file = file.path(OUTPUT_DIR, 'to_process.csv'), row.names = FALSE)

if(showMessages) message('script complete')
