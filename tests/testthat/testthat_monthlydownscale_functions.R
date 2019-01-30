
context('monthly downscaling')

## For some reason, setting cores to anything > 1 causes `R CMD check --as-cran`
## to fail.  Running without the cran setting works with larger core counts.
## The following test causes the parallel version to be tested when running
## locally, while still functioning when running under CRAN rules.
notcran <- Sys.getenv('NOT_CRAN') == 'true'
if(notcran) {
    doParallel::registerDoParallel(cores=4)
} else {
    doParallel::registerDoParallel(cores=1)
}

# we are going to want to test that this works with the tas, and pr, and that it does not work with some other things
# if it throws an error when the fraction has more than 12 months!
# will want to check that the outputs are expected
test_that('monthly downscaling', {

  # This fraction data frame has less than 12 entries for months
  frac1 <- list(tas = matrix(rep(5, 20), ncol = 4),
                pr = matrix(rep(2, 20), ncol = 4),
                coord = data.frame(column_index = 1:4,
                                         lat = 10:13,
                                         lon = rep(20, 4)),
                time = 2000:2004)

  # This fraction data frame has entries for each month
  frac2 <- list(tas = matrix(rep(5), ncol = 4, nrow = 12),
                pr = matrix(rep(2), ncol = 4, nrow = 12),
                coord = data.frame(column_index = 1:4,
                                         lat = 10:13,
                                         lon = rep(20, 4)),
                time = 2000:2004)

  an_temp_val <- 273
  an_pr_val <- 10
  fld <- list(coord = data.frame(column_index = 1:4,
                                       lat = 10:13,
                                       lon = rep(20, 4)),
              time = 2000:2004,
              fullgrids = list(tas = matrix(rep(an_temp_val, 20), ncol = 4),
                               pr = matrix(rep(an_pr_val, 20), ncol = 4)))

  # Monthly down scaling throws expected errors
  #
  # Make sure that the monthly downscaling function throws errors when there is some problem with the funciton inputs.
  expect_error(monthly_downscaling(frac1, fld$fullgrids[['tas']], fld$coord, fld$time, var = 'tas'), 'there must be 12 rows one for each month')
  expect_error(monthly_downscaling(frac1, fld$fullgrids$tas, fld$coord, fld$time, var = 'fake'), 'frac input missing fake')
  expect_error(monthly_downscaling(frac1, fld$fullgrids, fld$coord, fld$time, var = 'fake'), 'fld_data must be an array')


  # Temperature downscaling
  #
  # Check the temperature downscaled results
  tas_rslt <- monthly_downscaling(frac2, fld$fullgrids$tas, fld$coord, fld$time, var = 'tas')

  expect_length(tas_rslt, 2)
  expect_equal(tas_rslt$coord, fld$coord)
  expect_equal(nrow(tas_rslt$data), 12 * length(fld$time))

  ## Since the grids in the test data have the same mean and same Dirichlet
  ## parameters, they should have the same values (this will not be the case for
  ## two cells that have different annual values, _or_ different Dirichlet
  ## parameters).
  for(col in seq(1,ncol(fld$fullgrids$tas))) {
      expect_equal(tas_rslt$data[,1], tas_rslt$data[,col],
                   info=paste('In tas downscaling: discrepancy between column 1 and column', col))
  }

  ## Check that each year's mean is equal to the annual value.  Also, check that each
  ## year is different from its predecessor.
  for(yr in seq_along(fld$time)) {
    idx <- (yr-1)*12 + seq(1,12)
    annual_temp <- apply(tas_rslt$data[idx,], 2, mean)
    expect_equivalent(annual_temp, rep(an_temp_val, ncol(tas_rslt$data)),
                      info=paste('Average temperature not preserved in year', yr))
    if(yr > 1) {
      idx2 <- (yr-2)*12 + seq(1,12)
      expect_false(isTRUE(all.equal(tas_rslt$data[idx,], tas_rslt$data[idx2,])),
                   info=paste('Monthly fractions do not appear to be variable in years',
                             yr-1, 'and', yr))
    }
  }

  # Precipitation downscaling
  #
  # Check the units returned from the pr downscaling, this function should run without error.
  pr_rslt <- monthly_downscaling(frac2, fld$fullgrids$pr, fld$coord, fld$time, var = 'pr')

  expect_length(pr_rslt, 2)
  expect_equal(pr_rslt$coord, fld$coord)
  expect_equal(nrow(pr_rslt$data), 12 * length(fld$time))

  ## Check that each year's _mean_ is equal to the annual value.  Again, check that each
  ## year is different from its predecessor
  for(yr in seq_along(fld$time)) {
    idx <- (yr-1)*12 + seq(1,12)
    annual_pr <- apply(pr_rslt$data[idx,], 2, mean)
    expect_equivalent(annual_pr, rep(an_pr_val, ncol(pr_rslt$data)),
                      info=paste('Total precipitation not preserved in year', yr))
    if(yr > 1) {
      idx2 <- (yr-2)*12 + seq(1,12)
      expect_false(isTRUE(all.equal(pr_rslt$data[idx,], pr_rslt$data[idx2,])),
                   info=paste('Monthly fractions do not appear to be variable in years',
                             yr-1, 'and', yr))
    }
  }


  # Modify inputs and check outputs
  #
  # Modify only one of the alpha entries, by making it much larger than the others, we
  # should guarantee that the last month is always the largest (it happened 999 times out
  # of 1000 in testing; we will set the seed here to prevent random failures)
  frac3 <- frac2
  frac3$tas[12,] <- 25

  set.seed(8675309)
  tas_rslt2 <-  monthly_downscaling(frac3, fld$fullgrids$tas, fld$coord, fld$time, var = 'tas')
  for(yr in seq_along(fld$time)) {
    idx <- (yr-1)*12 + seq(1,12)
    for (gridcell in seq(1,ncol(tas_rslt2$data))) {
      expect_true(all(tas_rslt2$data[idx[12],gridcell] > tas_rslt2$data[idx[1:11], gridcell]),
                  info=paste('Month 12 not the largest for year', yr, 'gridcell', gridcell))
    }
  }
})


