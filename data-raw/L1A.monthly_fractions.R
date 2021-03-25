# Purpose: Calculate the average monthly fraction for each grid cell using cdos warpped in R.

# Notes: This script uses CDO and requires a datframe containting the netcdf files
# to process. The netcdfs paths may come from L0 output or may be provided
# by the user in section 1.

# Enhancements: for potential code enhancements search TODO in this script

# 0. Set Up -----------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ncdf4)


# Directories
BASE        <- "/pic/projects/GCAM/Dorheim/an2month"                               # The project directory
TEMP_OUTPUT <- "/pic/scratch/dorh012"                                             # Define a place to store the interminate netcdfs created during the cdo processing.
OUTPUT      <- file.path(BASE, 'data-raw', 'output-L1'); dir.create(OUTPUT)       # Define a place to store the final output netcdfs created
CDO_DIR     <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"                       # Define the cdo directory.


# 1. To process --------------------------------------------------------------------------
# Determine the netcdf files to process, here we are importing the ismip file.

 to_process <- readr::read_csv(file.path(BASE, 'data-raw', 'output-L0', 'isimip', 'to_process.csv')) %>%
   select(-date)


# 2. Define Functions  -------------------------------------------------------------------

# check_annualAvg_nc
# is a function used internally in monthly_fraction to check to make sure that length of time
# in the annual average is correct annual avearge.
check_annualAvg_nc <- function(concatenated_nc, annualAvg_nc){

  nc_open(concatenated_nc) %>%
    ncvar_get('time') %>%
    length ->
    concatFrac_time

  nc_open(annualAvg_nc) %>%
    ncvar_get('time') %>%
    length ->
    annualAvg_nc_time

  message('Checking annual average:')
  if(annualAvg_nc_time == concatFrac_time/12) {

    message('length of annual average nc file is correct')


  } else {

    stop('incorrect number of years in ', annualAvg_nc)

  }

}

# check_years_ncs
# is a function that randomly selects one of the fraction netcdfs for a single year then checks
# to make sure that there are only 12 months of data and that the sum of the fracations should
# meet the expected value. Since we are techincally looking at the ratio of the monthly
# average to annual average the sum should be 12.
check_years_nc <- function(years_nc, var, inter_dir){

  # Randomly select the a single nc file to check for the number of months and then check
  # the sum of the values.
  nc_path <- years_nc[sample(1:length(years_nc), size = 1)]

  # Check the length of time, should be 12
  nc_open(nc_path) %>%
    ncvar_get('time') %>%
    length ->
    frac_time

  if(frac_time == 12){

    message('12 months of data in the monthly fraction nc')

  } else {

    stop('problem with the number of months in the monthly fraction for ', nc_path)

  }

  # Check to see that the sum of the grid cells across the months equals 12.
  test_nc <- paste0(inter_dir, 'test_frac.nc')
  system2(CDO_DIR, args = c("yearsum", nc_path, test_nc), stdout = TRUE, stderr = TRUE)

  # Import the test nc and check to see if there is only one time setp of data.
  data_test <- nc_open(test_nc)
  if(length(ncvar_get(data_test, 'time')) != 1) stop('problem with the number of time steps in ', test_nc)

  # Check to see if there are any values that do not add up to 12
  var_sum <- ncvar_get(data_test, var)

  # Check to see the summary stats are for the sum of the fractions, the summary stats should equal 12
  sum_output <- summary(as.vector(var_sum))
  diff       <- sum_output[1:6] - 12

  if(any(abs(diff) > 1e-4)){

    print(sum_output)
    stop('unexpected values in ', test_nc, ' values expected to add up to 12.')


  } else {

    message('summary stats from ', test_nc)
    print(sum_output)

  }

  file.remove(test_nc)

}


# check_concatFrac
# is a function that checks the length of time in the concatenatedFraction netcdf, should euqal
# the concatenated netcdf.
check_concatenatedFrac_nc <- function(concatenated_nc, concatenatedFrac_nc){

  nc_open(concatenated_nc) %>%
    ncvar_get('time') %>%
    length ->
    concatFrac_time

  nc_open(concatenatedFrac_nc) %>%
    ncvar_get('time') %>%
    length ->
    concatenatedFrac_nc_time

  if(concatenatedFrac_nc_time == concatFrac_time) {

    message('the length of the monthly fraction concatenated file is correct')

  } else {

    stop('there is a problem with the length of monthly fraction concatenated file')

  }

}


# check__avgFrac_nc
# is a function that checks the length of time for the monthly average fraction (the final netcdf), is must
# be divisible by 12.
check_avgFrac_nc <- function(avgFrac_nc, var, inter_dir){

  nc_open(avgFrac_nc) %>%
    ncvar_get('time') %>%
    length ->
    avgFrac_time

  if(!is.integer(avgFrac_time / 12)) {

    stop('There is a problem with the number of time steps in ', avgFrac_nc)

    } else {

      message('Correct number of time steps in ', avgFrac_nc)

    }


  file.remove(test_nc)

}


# monthly_fraction
# is a fraction that calculates the monthly fraction
# of a value for some monthly netcdf variable / model / ensemble file(s). The
# idea is that this function can be lapply'd to a list of the CMIP 5 files
# to process.

# input - a tibble of netcdf information, requires a path column and some meta information (model, exeperiment, ect.)
  # This function will concatenate the files with the same netcdf meta information together (same model, experiment, variable).
# cdo_dir - the directory to the cdo
# intermediate_dir - the directory to save the intermediate nc files to
# output_dir - the directory to save the average monthly fraction nc files to
# showMessages - a Boolean for hiding messages about the function progress
# saveIntermediates - a Boolean to clean up or save the intermediate nc files
# testOUtputs- a Boolean to test all of the cdo outputs, note if set to true this will increase the function running time.

monthly_fraction <- function(input, cdo_dir, intermediate_dir, output_dir, showMessages = FALSE,
                             saveIntermediates = FALSE, testOutputs = FALSE){

  # Part 1 : check inputs and prepare to run cdo -----
  # This function depends on CDO (https://code.zmaw.de/projects/cdo) being installed
  stopifnot(file.exists(cdo_dir))

  # Input tibble requirements
  req_cols <- c('path', 'variable')
  missing  <- !req_cols %in% names(input)
  if(any(missing)) stop('input is missing ', paste(req_cols[missing], sep = ', ') )

  file_info <- distinct(select(input, -path))
  message(file_info)
  if(nrow(file_info) != 1) stop('input can only contain a single variable/domain/model/experiment/ensemble combination\n', file_info)

  # Required directories
  stopifnot(dir.exists(intermediate_dir))
  stopifnot(dir.exists(output_dir))

  # Make all of the intermediate nc files.
  inter_name  <- file.path(intermediate_dir, paste(file_info[1,], collapse = '_'))
  output_name <- file.path(output_dir, paste(file_info[1,], collapse = '_'))

  concatenated_nc     <- paste0(inter_name, '_concatenated.nc')
  annualAvg_nc        <- paste0(inter_name, '_annualAvg.nc')
  concatenatedFrac_nc <- paste0(output_name, '_monthlyFrac.nc')
  #avgFrac_nc          <- paste0(output_name, '_avgFrac.nc')

  # If the concatenated files exist they will cause problems
  if(file.exists(concatenated_nc)) stop(concatenated_nc, ' exists and cannot be over written')
  if(file.exists(concatenatedFrac_nc)) stop(concatenatedFrac_nc, ' exists and cannot be over written')

  # Part 2 : run cdo ------
  # Concatenate the netcdf files for the same model / variable / ensemble together.
  if(showMessages) message("Concatenating files and converting to absolute time ", concatenated_nc)
  system2(CDO_DIR, args = c("-a", "cat", input$path, concatenated_nc), stdout = TRUE, stderr = TRUE)

  # Calculate the annual average weighted by the number of days in each month.
  if(showMessages) message("Calculate the annual average ", annualAvg_nc)
  # to calculate the monthly to annual fracations.
  system2(CDO_DIR, args = c("yearmean", concatenated_nc, annualAvg_nc), stdout = TRUE, stderr = TRUE)


  # For each year divide the monthly values by the annual average
  # TODO there might be a better way to do this but for now use lapply
  # to divide monthly values by annual averages for each year. In the
  # following step we will concatenate the monthly fractions together.
  tibble(years = ncvar_get(nc_open(annualAvg_nc), 'time')) %>%  # Parse out the years from the netcdf
    mutate(years = substr(years, 1, 4),
           frac_file = paste0(inter_name, '_frac', years, '.nc'),
           mon_file = paste0(inter_name, '_monthyear', years, '.nc'),
           year_file = paste0(inter_name, '_year', years, '.nc')) ->
    years_ncs

  if(showMessages) message("Dividing monthly values by annual average")
  lapply(X = split(years_ncs, years_ncs$years),
         FUN = function(input = X){

           # Select the a single year of monthly data and then divide by annual average for the same year
           # if(showMessages) message("Dividing monthly values by annual average for ", input[['years']])

           # The ISMIP data is so large that we cannot pipe the cdo comands together without causing a segmentation
           # fault. Select the individual years and from the monthly and annual average netcdfs separetly and then
           # divide by one another to get the monthly average to annual average fraction.
           system2(CDO_DIR, args = c(paste0("selyear,", input[['years']]), concatenated_nc, input[['mon_file']]),
           stdout = TRUE, stderr = TRUE)

           system2(CDO_DIR, args = c(paste0("selyear,", input[['years']]), annualAvg_nc, input[['year_file']]),
           stdout = TRUE, stderr = TRUE)

           system2(CDO_DIR, args = c("div", input[['mon_file']], input[['year_file']], input[['frac_file']]),
                   stdout = TRUE, stderr = TRUE)

  })

  # Concatenate the monthly fractions together
  if(showMessages) message("Concatenate monthly fractions together")
  system2(CDO_DIR, args = c("cat", years_ncs$frac_file, concatenatedFrac_nc), stdout = TRUE, stderr = TRUE)


  # Part 3: Run the code tests ----
  # Check the various intermediate to make sure the data looks good. This will increase the function
  # run time but will provide useful information for troubble shooting problems.
  if(testOutputs){
    message('testing outputs')

    check_annualAvg_nc(concatenated_nc, annualAvg_nc)

    check_years_nc(years_ncs$frac_file, file_info$variable, TEMP_OUTPUT)

    check_concatenatedFrac_nc(concatenated_nc, concatenatedFrac_nc)

    message('passes all tests \n\n')
  }


  # Part 4: Clean up and return output -----
  if(!saveIntermediates){

    # If the saveIntermediates is set to FALSE then remove the intermediate netcdf files
    file.remove(
                annualAvg_nc,
                years_ncs$frac_file,
                years_ncs$mon_file,
                frac_file$year_file)

  }

  concatenatedFrac_nc

}


# 3. Get Monthly Fractions  -------------------------------------------------------------------

# Split up the to_process list by isimip information,
input_list <- split(to_process, interaction(to_process$model, to_process$variable, to_process$experiment))


# lapply the monthly_fraction function to the list of the cmip files to
# process.
lapply(X = input_list,
       FUN = monthly_fraction,
       cdo_dir = CDO_DIR,
       intermediate_dir = TEMP_OUTPUT,
       output_dir = OUTPUT,
       showMessages = TRUE,
       saveIntermediates = TRUE,
       testOutputs = FALSE) ->
  average_monthly_fractions

# End



