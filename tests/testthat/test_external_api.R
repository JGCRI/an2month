context('External API functions')


test_that('External API produces valid fields', {

    ncell <- 10
    ntime <- 5
    nmonth <- 12*ntime
    nfield <- 3

    ## Find 10 representative grid cells to use in our test
    testcells <- which(!is.na(alpha_ipsl_cm5a_lr$tas[1, ]))[1:ncell]

    ## make some field data
    fields <-
        lapply(seq(nfield),
               function(i) {
                   matrix(10*i, nrow=ntime, ncol=ncell)
               })

    ## Construct the coordinate data
    coord <- as.matrix(alpha_ipsl_cm5a_lr$coord[testcells, c('lat','lon')])

    ## time vector
    time <- 2005 + seq(ntime)


    for(var in c('tas','pr')) {
        monthly_fields <-
            downscaling_component_api('alpha_ipsl_cm5a_lr', fields, coord, time,
                                      var, 1)
        ## Check that the fields are correct
        expect_length(monthly_fields, nfield)
        for(i in seq(length(monthly_fields))) {
            mfield <- monthly_fields[[i]]
            expect_equal(dim(mfield), c(nmonth, ncell))

            avgval <- rep(i*10, ncol(mfield))
            expect_equal(apply(mfield, 2, mean), avgval,
                         info='mean temperature not equal to expected mean')


            ## Check that the precip transform can be applied, if this is a
            ## precip field.
            if(var == 'pr') {
                mfield_mm_mon <- expect_silent(pr_conversion(mfield))
                expect_true(is.matrix(mfield_mm_mon))
                expect_equal(dim(mfield_mm_mon), c(nmonth, ncell))
            }
        }
    }
})
