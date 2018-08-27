# Purpose: Calculate the average monthtly fraction for each grid cell.

# Notes: This script uses CDO and requires a the paths to the CMIP 5 files
# to process. The CMIP 5 file paths may come from L0 output or may be provided
# by the user in section 1.



# 0. Set Up -----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)


# Directories
BASE        <- "/pic/projects/GCAM/Dorheim/grand_exp/an2month"  # The project directory
TEMP_OUTPUT <- "/pic/scratch/dorh012"                           # Define a place to store the interminate netcdfs created during the cdo processing.
CDO_DIR     <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"     # Define the cdo directory.


