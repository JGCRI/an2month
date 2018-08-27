# Purpose: this script finds CMIP5 files that will be processed in L1 that will
# be used as inputs for the monthly fraction processing code in L1. This script
# returns csv files that contain CMIP5 netcdf files to process and information
# (model / variable / ensemble member) about the files to be processed in L1.

# Note: This script is not necessary if the user knows the paths of the CMIP 5
# files to process the user may start at L1.

# User Decisions: In section 1 the user can decide what CMIP5 files to search for,
# the more CMIP5 files identified to process the longer this script and subsequent
# data-raw scripts will take to run.


# 0. Set Up -----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# Define the directories.
BASE         <- "/pic/projects/GCAM/Dorheim/grand_exp/an2month"   # The project location.
CMIP5_DATA   <- "/pic/projects/GCAM/CMIP5-CHartin"                # Directory containing the cmip5 files to process.
OUTPUT_DIR   <- file.path(BASE, "data-raw", "input")              # Define the output directory.
dir.create(OUTPUT_DIR, showWarnings = FALSE)


# 1. User Decisions -----------------------------------------------------------------------------------------------------
# Select the CMIP5 variables, experiments, ensembles, realms, and time scales
# to process. These vectors will be used to generate a pattern that will select cmip5
# files of interest from the CMIP5_DATA.  If a vector is set to NULL then the pattern
# will match with any variable, experiment, ensemble, domain pattern.

# cmip5 search vectors
VARIABLES       <- c("tas", "pr")            # CMIP5 variables to process
EXPERIMENTS     <- NULL                      # CMIP5 experiments to process
ENSEMBLES       <- "[a-zA-Z0-9-]{4}p1"       # search for an ensemble with p1
DOMAINS         <- "Amon"                    # the modeling domain name
MODELS          <- "CESM1-CAM5"              # CMIP5 models to process


# 2. Find the CMIP5 files to process -----------------------------------------------------------------------------------------------------

# pattern_gen is a function that converts the cmip5 search vectors from section
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
domaninpattern    <- pattern_gen(DOMAINS)
modelpattern      <- pattern_gen(MODELS)
experimentpattern <- pattern_gen(EXPERIMENTS)
ensemblepattern   <- pattern_gen(ENSEMBLES)


# Create the pattern for CMIP5 file names for the cmip 5 files to search for.
cmip_search_pattern <- paste("(", varpattern, ")_(",
                             domaninpattern, ")_(",
                             modelpattern, ")_(",
                             experimentpattern, ")_(",
                             ensemblepattern,
                             ")_([0-9]{6})-([0-9]{6}", # time, set up to search for any 6 digit date
                             ").nc$", sep = "")
# Sanity check
# message("CMIP5 variable search pattern: ", cmip_search_pattern, "\n")


# Search for the files
file_list <- list.files(CMIP5_DATA, pattern = cmip_search_pattern, full.names = TRUE, recursive = TRUE)

if(length(file_list) < 1) stop('Could not find any cmip files files matching ', cmip_search_pattern)

# Parse out the CMIP% meta data information from the file name, this will be used to
# make a table to help illustrate what we have and which ones are missing.
tibble(path = file_list) %>%
  mutate(filename = basename(path)) %>%
  separate(filename, into = c("variable", "domain", "model", "experiment",
                              "ensemble", "date"), sep = "_", remove = FALSE) %>%
  select(path, variable, domain, model, experiment, ensemble) ->
  CMIP5_to_process
q()

# 3. Save output -----------------------------------------------------------------------------------------------------

# Save the tibble of the CMIP 5 files to use to process in L1.
write.csv(CMIP5_to_process, file = file.path(OUTPUT_DIR, "CMIP5_to_process.csv"), row.names = F)
message('Finishing up L0.find_cmip5_files and saving results to...\n ',
        file.path(OUTPUT_DIR, "CMIP5_to_process.csv"))


