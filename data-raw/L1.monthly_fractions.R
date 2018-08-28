# Purpose: Calculate the average monthly fraction for each grid cell.

# Notes: This script uses CDO and requires a the paths to the CMIP 5 files
# to process. The CMIP 5 file paths may come from L0 output or may be provided
# by the user in section 1. Currently set up in a "devlopment mode" will need
# to remove [1] in line 149

# Enhancements: for potential code enhancements search TODO in this script

# 0. Set Up -----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ncdf4)


# Directories
BASE        <- "/pic/projects/GCAM/Dorheim/grand_exp/an2month"                 # The project directory
TEMP_OUTPUT <- "/pic/scratch/dorh012"                                          # Define a place to store the interminate netcdfs created during the cdo processing.
OUTPUT      <- "/pic/projects/GCAM/Dorheim/grand_exp/an2month/data-raw/output" # Define a place to store the final output netcdfs created
CDO_DIR     <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"                    # Define the cdo directory.


# 1. To process -------------------------------------------------------------------
# Determine the CMIP 5 files to process, this come from the L0 output.

to_process <- readr::read_csv(file.path(BASE, 'data-raw', 'input', 'CMIP5_to_process.csv'))


# 2. Get monthly fraction -------------------------------------------------------------------

# TODO format into a function that can be lapplyed!
# Will want to state that we can do the piping at another time.
# I will probably want to include something to show that the code is correct.
# Proof of concept example! after the first time i run it, if the code
# is written well should be something that I can rerun on all of the intermediate
# files to check my work
# There is going to be a bunch of post processing that will need to take place but
# I don't have to worry about that now
# There might be a better way to handle the years... .that is an enhancement down the line


# monthly_fraction
# is a fraction that calculates the average monthly fraction
# of a value for some monthly CMIP 5 variable / model / ensemble file(s). The
# idea is that this function can be lapply'd to a list of the CMIP 5 files
# to process.

# input - a tibble of CMIP 5 information, requires a path, model, ensemble,
  # variable, variable columns. This function will concatenate the files
  # corresponding the input$path column together.
# cdo_dir - the directory to the cdo
# intermediate_dir - the directory to save the intermediate nc files to
# output_dir - the directory to save the average monthly fraction nc files to
# showMessages - a Boolean for hiding messages about the function progress
# saveIntermediates - a Boolean to clean up or save the intermediate nc files

monthly_fraction <- function(input, cdo_dir, intermediate_dir, output_dir, showMessages = FALSE,
                             saveIntermediates = FALSE){

  # Part 1 : check inputs and prepare to run cdo -----
  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed
  stopifnot(file.exists(cdo_dir))

  # Input tibble requirements
  req_cols <- c('path', 'variable', 'domain', 'model', 'experiment', 'ensemble')
  missing  <- !req_cols %in% names(input)
  if(any(missing)) stop('input is missing ', paste(req_cols[missing], sep = ', ') )

  cmip_info <- distinct(select(input, -path))
  if(nrow(cmip_info) != 1) stop('input can only contain a single variable/domain/model/experiment/ensemble combination\n', cmip_info)

  # Required directories
  stopifnot(dir.exists(intermediate_dir))
  stopifnot(dir.exists(output_dir))

  # Make all of the intermediate nc files.
  inter_name  <- file.path(intermediate_dir, paste(cmip_info[1,], collapse = '_'))
  output_name <- file.path(output_dir, paste(cmip_info[1,], collapse = '_'))

  concatenated_nc     <- paste0(inter_name, '_concatenated.nc')
  annualAvg_nc        <- paste0(inter_name, '_annualAvg.nc')
  concatenatedFrac_nc <- paste0(inter_name, '_concatenatedFrac.nc')
  avgFrac_nc          <- paste0(output_name, '_avgFrac.nc')

  # If the concatenated files exist they will cause problems
  if(file.exists(concatenated_nc)) stop(concatenated_nc, ' exists and cannot be over written')
  if(file.exists(concatenatedFrac_nc)) stop(concatenatedFrac_nc, ' exists and cannot be over written')

  # Part 2 : run cdo ------
  # Concatenate the cmip files for the same model / variable / ensemble together.
  if(showMessages) message("Concatenating files and converting to absolute time ", concatenated_nc)
  system2(CDO_DIR, args = c("-a", "cat", input$path, concatenated_nc), stdout = TRUE, stderr = TRUE)

  # Calculate the annual average weighted by the number of days in each month.
  if(showMessages) message("Calculate the annual average ", annualAvg_nc)
  system2(CDO_DIR, args = c("yearmean", concatenated_nc, annualAvg_nc), stdout = TRUE, stderr = TRUE)

  # For each year divide the monthly values by the annual average
  # TODO there might be a better way to do this but for now use lapply
  # to divide monthly values by annual averages for each year. In the
  # following step we will concatenate the monthly fractions together.
  tibble(years = ncvar_get(nc_open(annualAvg_nc), 'time')) %>%  # Parse out the years from the netcdf
    mutate(years = substr(years, 1, 4),
           name = paste0(base_name, '_frac', years, '.nc')) ->
    years_ncs

  lapply(X = split(years_ncs, years_ncs$years),
         FUN = function(input = X){

           # Select the a single year of monthly data and then divide by annual average for the same year
           if(showMessages) message("Dividing monthly values by annual average for ", input[['years']])
           system2(CDO_DIR, args = c('div', paste0("-selyear,", input[['years']]), concatenated_nc, paste0("-selyear,", input[['years']]), annualAvg_nc, input[['name']]),
            stdout = TRUE, stderr = TRUE)

  })

  # Concatenate the monthly fractions together
  if(showMessages) message("Concatenate monthly fractions together")
  system2(CDO_DIR, args = c("cat", years_ncs$name, concatenatedFrac_nc), stdout = TRUE, stderr = TRUE)

  # Multi year monthly average
  if(showMessages) message("Multi year monthly average")
  system2(CDO_DIR, args = c("ymonavg", concatenatedFrac_nc, avgFrac_nc), stdout = TRUE, stderr = TRUE)

  # Part 3: Clean up and return output -----
  if(!saveIntermediates){

    # If the saveIntermediates is set to FALSE then remove the intermediate netcdf files
    file.remove(concatenated_nc,
                annualAvg_nc,
                concatenatedFrac_nc,
                years_ncs$name)

  }

  avgFrac_nc

}


# Split up the to_process list by CMIP information (everything but path)
# so that if there are models that split up results into different periods
# the monthly_fraction can concatenate the ncs together before processing.
cmip_list <- split(to_process,
                   interaction(to_process$model, to_process$ensemble,
                               to_process$variable, to_process$experiment,
                               to_process$domain))[1] # TODO the [1] should be removed when finished devloping code.

# lapply the monthly_fraction function to the list of the cmip files to
# process.
lapply(X = cmip_list, FUN = monthly_fraction,
       cdo_dir = CDO_DIR,
       intermediate_dir = TEMP_OUTPUT,
       output_dir = OUTPUT,
       showMessages = TRUE,
       saveIntermediates = TRUE) ->
  average_monthly_fractions

# Next steps
  # We are going to want something that is a proof of concept, that
  # can check the intermediate files to prove that cdo does what
  # we think it does, this might also come in handy if we run into errors :(

  # We are going to want to add some code that formats the results, into
  # from nc to something that can be saved as an R data file. Also will need
  # to figure out the xanthos grid system.



