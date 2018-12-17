context('External API functions')


test_that('External API produces valid fields', {

    ncell <- 10
    ntime <- 5
    nmonth <- 12*ntime
    nfield <- 3

    ## Find 10 representative grid cells to use in our test
    testcells <- which(!is.na(frac_ipsl_cm5a_lr$tas[1, ]))[1:ncell]

    ## make some field data
    fields <-
        lapply(seq(nfield),
               function(i) {
                   matrix(10*i, nrow=ntime, ncol=ncell)
               })

    ## Construct the coordinate data
    coord <- as.matrix(frac_ipsl_cm5a_lr$coordinates[testcells, c('lat','lon')])

    ## time vector
    time <- 2005 + seq(ntime)


    for(var in c('tas','pr')) {
        monthly_fields <-
            downscaling_component_api('frac_ipsl_cm5a_lr', fields, coord, time, var)
        ## Check that the fields are correct
        expect_equal(length(monthly_fields), nfield)
        for(i in seq(length(monthly_fields))) {
            mfield <- monthly_fields[[i]]
            expect_equal(dim(mfield), c(ncell, nmonth))

            avgval <- rep(i*10, nrow(mfield))
            expect_equal(apply(mfield, 1, mean), avgval)
        }
    }
})
