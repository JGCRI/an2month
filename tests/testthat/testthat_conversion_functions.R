
context('conversion functions')


test_that('tas_conversion', {

  # Check to see that it works as expected with a single input
  input <- 273
  out   <- tas_conversion(input)

  expect_equal(input, out +  273.15)

  # Check to see that it works with a matrix input
  input <- matrix(data = rep(273), nrow = 4, ncol = 5)
  out   <- tas_conversion(input)

  expect_equal(unique(as.vector(out)), -0.15)

})


test_that('pr_conversion', {

  # Create a matrix of values with the YYYYMM as row names.
  pr <- matrix(10, nrow = 5, ncol = 5)
  row.names(pr) <- c(200001:200005)

  # Convert pr, then check the output.
  out <- pr_conversion(pr)

  # Since we constructed a pr matrix with constant values we expect the rows to have a
  # single unique value, so the unique rows should be a vector equal in length to the
  # time element of pr (the columns).
  unique_rows <- apply(out, 1, unique)
  expect_null(dim(unique_rows))
  expect_equal(length(unique_rows), ncol(pr))


  # Now check the entries in the first column, it should equal 10 * the number of seconds in the
  # the time step.
  #
  # Calculate the expect number of seconds in each month.

  dpm <- c(31, 29, 31, 30, 31)          # days per month.
  spm <- dpm * 86400                    # 86400 seconds per day
  expected_col <- 10*spm                # original input was 10

  expect_equal(as.vector(out[,1]), expected_col)


  # Expect Errors
  # If the time of the input is not the YYYYMM format
  pr2 <- pr
  row.names(pr2) <- paste0(row.names(pr), '01')
  expect_error(pr_conversion(pr2), 'row.names of input must be YYYYMM format')

})
