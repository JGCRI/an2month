
# Test that the internal checking functions work as expected.
context('internal functions')

# Test that the check_names function throws errors only when expected.
test_that('check names', {

  df <- data.frame('HI' = c(1), 'DC' = c(2))

  # No errors should be thrown here
  check_names(df, req_names = c('HI', 'DC'))
  check_names(df, req_names = c('HI'))
  check_names(df, req_names = c('DC'))
  check_names(df, req_names = c('DC'), 'testthat df')


  expect_error(check_names(df, req_names = c('MN')), ' missing MN')
  expect_error(check_names(df, req_names = c('MN'), 'testthat df'), 'testthat df missing MN')

  help(testthat)


})

# Test that the harmonize function throws errors when expected and returns the correct out put.
test_that('harmonize coordinates', {

  frac <- list(tas = matrix(rep(1/5,20), ncol = 4),
               pr = matrix(rep(1/2,20), ncol = 4),
               coordinates = data.frame(column_index = 1:4,
                                        lat = 10:13,
                                        lon = rep(20, 4)),
               time = 2000:2004)

  # Make sure that the harmonize coordinates function throws the errors that we are expecting
  fld_coord1 <- data.frame(column_index = 1:4, lat = 5:6, lon = 20)
  expect_error(harmonize_coordinates(frac = frac, frac_coordinates = frac$coordinates, fld_coordinates = fld_coord1, var = 'tas'),
               'fraction file is missing lat values')

  fld_coord2 <- data.frame(column_index = 1:4, lat =  10:13, lon = 19)
  expect_error(harmonize_coordinates(frac = frac, frac_coordinates = frac$coordinates, fld_coordinates = fld_coord2, var = 'tas'),
               'fraction file is missing lon values')

  fld_coord3 <- data.frame(column_index = 1:9, lat = 10:18, lon = 20)
  expect_error(harmonize_coordinates(frac = frac, frac_coordinates = frac$coordinates, fld_coordinates = fld_coord3, var = 'tas'),
               'Resolution of the monthly fraction coordinates is insufficient')

  # Make sure that fraction matrix returned is correct.
  # Recall that the grid cells are harmonized by coordinates not by column index so select by changing the lat in the field coordinates
  # data frame.
  fld_coord4 <- data.frame(column_index = 1:2, lat = c(11,13), lon = 20)
  # Modify one of the fraction values to make sure that we are selecting the correct grid cells with this function
  frac$tas[1, 4] <- 10
  frac$tas[2, 2] <- 6
  new_frac <- harmonize_coordinates(frac = frac, frac_coordinates = frac$coordinates, fld_coordinates = fld_coord4, var = 'tas')

  # We only expect the number of columns to match the number of rows in the field coordinates data frame.
  expect_equal(ncol(new_frac), nrow(fld_coord4))

  # The values we added to the frac matrix should both be in the new fraction matrix returned by the function.
  expect_equal( c(6, 10) %in% new_frac, c(TRUE, TRUE))

})
