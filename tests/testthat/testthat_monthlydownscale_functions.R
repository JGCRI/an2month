
context('monthly downscaling')


# we are going to want to test that this works with the tas, and pr, and that it does not work with some other things
# if it throws an error when the fraction has more than 12 months!
# will want to check that the outputs are expected
test_that('monthly downscaling', {

  # This fraction data frame has less than 12 entries for months
  frac1 <- list(tas = matrix(rep(1/5, 20), ncol = 4),
                pr = matrix(rep(1/2, 20), ncol = 4),
                coordinates = data.frame(column_index = 1:4,
                                         lat = 10:13,
                                         lon = rep(20, 4)),
                time = 2000:2004)

  # This fraction data frame has entries for each month
  frac2 <- list(tas = matrix(rep(1/5), ncol = 4, nrow = 12),
                pr = matrix(rep(1/2), ncol = 4, nrow = 12),
                coordinates = data.frame(column_index = 1:4,
                                         lat = 10:13,
                                         lon = rep(20, 4)),
                time = 2000:2004)

  fld <- list(coordinates = data.frame(column_index = 1:4,
                                       lat = 10:13,
                                       lon = rep(20, 4)),
              time = 2000:2004,
              fullgrids = list(tas = matrix(rep(273, 20), ncol = 4),
                               pr = matrix(rep(10, 20), ncol = 4)))

  # Monthly down scaling throws expected errors
  #
  # Make sure that the monthly downscaling function throws errors when there is some problem with the funciton inputs.
  expect_error(monthly_downscaling(frac1, fld$fullgrids[['tas']], fld$coordinates, fld$time, var = 'tas'), 'there must be 12 rows one for each month, in the frac input')
  expect_error(monthly_downscaling(frac1, fld$fullgrids$tas, fld$coordinates, fld$time, var = 'fake'), 'frac input missing fake')
  expect_error(monthly_downscaling(frac1, fld$fullgrids, fld$coordinates, fld$time, var = 'fake'), 'fld_data must be an array')


  # Temperature downscaling
  #
  # Check the temperature downscaled results
  tas_rslt <- monthly_downscaling(frac2, fld$fullgrids$tas, fld$coordinates, fld$time, var = 'tas')

  expect_equal(length(tas_rslt), 2)
  expect_equal(dim(tas_rslt$coordinates), dim(fld$coordinates))
  expect_equal(nrow(tas_rslt$data), 12 * length(fld$time))

  # Since we only used one fraction and value we can expect that the function only returns one value
  expect_equal(unique(as.vector(tas_rslt$data)), (frac2$tas[1] * fld$fullgrids$tas[1]))


  # Precipitation downscaling
  #
  # Check the units returned from the pr downscaling, this function should run without error.
  pr_rslt <- monthly_downscaling(frac2, fld$fullgrids$pr, fld$coordinates, fld$time, var = 'pr')


  # Modify inputs and check outputs
  #
  # Modify only one of the fraction entries, this means that for the results we should see 2 unique values,
  # one should only appear the number of years times.
  frac3 <- frac2
  frac3$tas[12] <- 1/8

  tas_rslt2 <-  monthly_downscaling(frac3, fld$fullgrids$tas, fld$coordinates, fld$time, var = 'tas')
  unique_values <- unique(as.vector(tas_rslt2$data))

  expect_equal(length(unique_values), 2)
  expect_equal(sum(as.vector(tas_rslt2$data) == unique_values[2]), length(fld$time))

  # What about if a grid cell has the same fraction value for all of the months?
  frac3$tas[ , 1] <- 1/20
  frac3$tas[ , 2] <- 1/10
  frac3$tas[ , 3] <- 1/30
  frac3$tas[ , 4] <- 1/40
  tas_rslt3     <-  monthly_downscaling(frac3, fld$fullgrids$tas, fld$coordinates, fld$time, var = 'tas')
  unique_values <- unique(as.vector(tas_rslt3$data))

  expect_equal(length(unique_values), 4)
  expect_equal(sum(as.vector(tas_rslt3$data) == unique_values[1]), length(fld$time) * 12)

})


