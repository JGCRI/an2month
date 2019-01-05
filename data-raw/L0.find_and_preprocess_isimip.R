
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

BASE         <- "/pic/projects/GCAM/Dorheim/grand_exp/an2month"     # The project location on pic
OUTPUT_DIR   <- file.path(BASE, "data-raw", "output-L0", "isimip")  # Define the output directory
CDO_DIR      <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"        # Define the cdo directory
ISIMIP_DIR   <- "/pic/projects/GCAM/leng569/data/Input_GCM_bced"    # The location of the isimip files on pic

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


# 1. User Decisions -----------------------------------------------------------------------------------------------------
# Select the ismip varaiables, experiment, model, and so on to process. The strings listed in each of the
# vectors will be used in a serach pattern to identify the files to process from the ISIMIP_DIR. We are interested
# in the biased correct files and they have the following file pattern. If the vector is set to NULL then the
# pattern will serach for all options.
# variable_bced_1960_1999_model_experiment_startYr-endYr.nc4
if(showMessages) message('1. User Decisions')


# isimip search vectors
VARIABLES       <- c("tas", "pr")           # isimip variables to process
EXPERIMENTS     <- "rcp4p5"                 # isimip experiments to process
MODELS          <- NULL                     # isimip models to process, so everything


# 2. Find the isimip files to process -----------------------------------------------------------------------------------------------------
# Create the serach netcdf search pattern and then serach the isimip directory.
if(showMessages) message('2. Find the isimip files to process')

# pattern_gen is a function that converts the isimip search vectors from section
# into strings that will be used to create the regex search pattern. If the
# input vector is set to NULL then the function will return a search all pattern.
pattern_gen <- function(vector){

  if(is.null(vector)){
    "[a-zA-Z0-9-]+"
  } else {
    paste(vector, collapse = "|")
  }

}

varpattern        <- pattern_gen(VARIABLES)
modelpattern      <- pattern_gen(MODELS)
experimentpattern <- pattern_gen(EXPERIMENTS)


# Create the pattern for the isimip file names for the netcdfs to search for.
isimip_search_pattern <- paste("(", varpattern,
                               ")_(bced)_([0-9]{4})_([0-9]{4})_(", # the biased corrected portion of the pattern
                               modelpattern, ")_(",
                               experimentpattern, ")_",
                               "([0-9]{4})-([0-9]{4}",          # time, set up to search for any 4 digit date
                               ").nc4$", sep = "")
# Sanity check
if(showMessages) message("isimip file search pattern: ", isimip_search_pattern, "\n")


# Search for the files
file_list <- list.files(ISIMIP_DIR, pattern = isimip_search_pattern, full.names = TRUE, recursive = TRUE)

if(length(file_list) < 1) stop('Could not find any isimip files files matching ', isimip_search_pattern)


# Parse out the isimip meta data information from the file name, this will be use to make a table to
# help illustrate what we have and identify missing netcdf files.
tibble(path = file_list) %>%
  mutate(filename = basename(path)) %>%
  separate(filename, into = c("variable", "A", "B", "C", "model",
                              "experiment", "date"), sep = "_", remove = FALSE) %>%
  select(path, variable, model, experiment, date) %>%
  separate(date, into = c("startYr", "endYr"), sep = "-", remove = TRUE) %>%
  mutate(endYr = gsub(".nc4", "", endYr)) ->
  isimip_file_info


# 3. Check coverage  ----------------------------------------------------------------------
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


# 4. Process to monthly  -------------------------------------------------------------------
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

# 5. Save nc information  -------------------------------------------------------------------
# Create a tibble to save as a csv file that contains the paths and some additional information
# about the nc files to pass on to level 1 to calculate the monthly fraction data.
if(showMessages) message('4. Save nc information')

tibble(file = unlist(ncs)) %>%
  mutate(filename = basename(file)) %>%
  separate(filename, into = c("monthly", "variable", "A", "B", "C", "model",
                              "experiment", "date"), sep = "_", remove = FALSE) %>%
  select(path = file, variable, model, experiment, date) ->
  to_process

write.csv(x = to_process, file = file.path(OUTPUT_DIR, 'to_process.csv'), row.names = FALSE)

if(showMessages) message('script complete')
