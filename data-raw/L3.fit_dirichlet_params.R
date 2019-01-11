#### Given the monthly fractions for the input variables by year and grid cell,
#### fit the alpha parameters for a Dirichlet distribution.

library(doParallel)
library(rstan)
library(assertthat)
library(usethis)
library(tibble)
library(ncdf4)

### Models available
modelnames <-
    c('gfdl-esm2m_rcp4p5',
      'ipsl-cm5a-lr_rcp4p5',
      'miroc-esm-chem_rcp4p5',
      'noresm1-m_rcp4p5',
      'test')

### Model fitting function.  The intention is to launch a separate job for each
### file.
dirfit <- function(infile, stanfile='dirichlet-fit.stan', test=FALSE)
### :param infile: RDS file containing the input data structure, a
###                matrix[nyear, nmonth, ngrid] of ESM output (either
###                temperature or precip).
### :param stanfile: File containing the stan code for fitting the Dirichlet
###                parameters.
### :param test: Flag. If true, the check on the number of grid cells is disabled.
{
    monfrac <- readRDS(infile)

    ## Find the valid cells.
    goodcells <- which(apply(monfrac, 3, function(v) {!is.na(v[1])}))
    ## Check that all the good cells have valid data and none of the bad cells
    ## do.  Also, there should be exactly 67420 good cells.
    if(!test) {
      assert_that(length(goodcells) == 67420)
    }
    assert_that(!any(is.na(monfrac[ , ,goodcells])))
    assert_that(all(is.na(monfrac[ , , -goodcells])))

    ## Compile the stan model
    stanmod <- stan_model(stanfile)

    nyear <- dim(monfrac)[1]

    alpha_fit <-
        foreach(igrid=goodcells, .combine=simplify2array) %dopar% {
            obs <- monfrac[, , igrid]
            ## The monthly values don't necessarily all sum to 1 (e.g. for
            ## temperature they should sum to something close to 12).  Normalize
            ## them appropriately.
            asuminv <- 1.0 / apply(obs, 1, sum)
            obsnorm <- obs * asuminv

            fit <- sampling(stanmod, data=list(Nyear=nyear, monfrac_obs=obsnorm),
                            refresh=0, chains=1)
            fitdata <- extract(fit)
            ## Return the expectation of the alpha value found in the simulation
            apply(fitdata$alpha, 2, mean)
        }

    ## Use monfrac as the skeleton for building our output
    alpha <- monfrac
    alpha[ , , goodcells] <- alpha_fit

    alpha
}


### Pull metadata from the netcdf files.
nccoord <- function(infile)
{
    nc <- nc_open(infile)

    time <- ncvar_get(nc, 'time')
    lat <- ncvar_get(nc, 'lat')
    lon <- ncvar_get(nc, 'lon')

    nlat <- length(lat)
    nlon <- length(lon)
    ngrid <- nlat*nlon

    latvals <- rep(lat, nlon)
    lonvals <- rep(lon, rep(nlat, nlon))
    index <- seq(1, ngrid)

    tibble(column_index=index, lat=latvals, lon=lonvals)
}


### Process a model
procmodel <- function(modelidx, test=FALSE)
### :param modelidx: Index into the modelnames array above
{
    model <- modelnames[modelidx]

    ## Precipitation fractions
    fracdata_pr <- file.path('output-L2', paste0('pr_', model,
                                                 '_monthlyFrac.rds'))
    alpha_pr <- dirfit(fracdata_pr, test=test)

    ## Temperature fractions
    fracdata_tas <- file.path('output-L2', paste0('tas_', model,
                                                  '_monthlyFrac.rds'))
    alpha_tas <- dirfit(fracdata_tas, test=test)

    ## Create coordinate structure
    ncfile <- file.path('output-L1', paste0('pr_', model, '_monthlyFrac.nc'))
    coord <- nccoord(ncfile)

    ## The 'time' element of the final output
    mnames <- dimnames(alpha_pr)[[2]]
    mnum <- sprintf('%02d', seq_along(mnames))
    time <- tibble(month=mnum, month_name=mnames)

    output_var <- gsub('_rcp4p5', '', model)
    output_var <- gsub('-','_', output_var)
    output_var <- paste0('frac_', output_var)

    outdata <- list(tas=alpha_tas, pr=alpha_pr, coord=coord, time=time)

    assign(output_var, outdata)

    message('Output var is', output_var)
    do.call(use_data, list(as.name(output_var), overwrite=TRUE, compress='xz'))
    invisible(outdata)
}

### Report on the statistics of monthly fractions in our datasets.
### This is a diagnostic tool, not part of the actual processing.
fracstat <- function(infile)
{
    monfrac <- readRDS(infile)

    goodcells <- which(apply(monfrac, 3, function(v) {!is.na(v[1])}))

    monfrac <- monfrac[ , , goodcells]

    meanfrac <- apply(monfrac, c(2,3), mean)
    sdfrac <- apply(monfrac, c(2,3), sd)
    list(meanfrac=meanfrac, sdfrac=sdfrac)
}

