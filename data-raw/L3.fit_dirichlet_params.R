#### Given the monthly fractions for the input variables by year and grid cell,
#### fit the alpha parameters for a Dirichlet distribution.
message('-----------------------------------------------------------------------------------')
library(doParallel)
#install.packages('rstan')
library(rstan)
library(assertthat)
#install.packages('usethis')
#library(usethis)
library(tibble)
library(ncdf4)
library(readr)
message('----------------------------------------------------------------------------------')

### Models available
model <- c('GFDL-ESM2M', 'HadGEM2-ES', 'IPSL-CM5A-LR', 'MIROC5')
rcps <-c('historical', 'rcp26', 'rcp45', 'rcp60', 'rcp85')

models <- rep(model, each =length(rcps))
rcps <- rep(rcps, length(model))
modelnames <- paste(models, rcps, sep = '_')
modelnames <- c(modelnames, 'test')

### Function to read the monthly data from a file
### :param filename: Name of a netcdf file
### :param varname: Name of the variable to read from the file
### :return: matrix[year, month, gridcell] of values
readnc <- function(filename, varname=NULL) {
  nc <- nc_open(filename)
  data <- ncvar_get(nc, varname)
  lat <- ncvar_get(nc, 'lat')
  lon <- ncvar_get(nc, 'lon')
  time <- ncvar_get(nc, 'time')
  nc_close(nc)

  nlat <- length(lat)
  nlon <- length(lon)
  ntime <- length(time)

  assert_that(all(dim(data) == c(nlon, nlat, ntime)))
  assert_that(ntime %% 12 == 0, msg='Non-integer number of years in data')
  nyear <- ntime / 12

  data <- aperm(data, c(3,2,1))
  dim(data) <- c(12, nyear, nlat*nlon)
  aperm(data, c(2,1,3))                 # put into [year, month, gridcell] order
}

### Model fitting function.  The intention is to launch a separate job for each
### file.
dirfit <- function(monfrac, outdir, stanfile='dirichlet-fit.stan', test=0)
### :param infile: matrix[nyear, nmonth, ngrid] of monthly fractions of ESM
###                output (either temperature or precip).
### :param outdir: directory to write checkpoint files into
### :param stanfile: File containing the stan code for fitting the Dirichlet
###                parameters.
### :param test: Flag. If true, the check on the number of grid cells is disabled.
{
    # Only keep 2006-2099
    ## TODO if ever want to fit distribution to historical
    ## fractions, would have to update.
    monfrac <- monfrac[1:94, ,]


    ## Find the valid cells.
    goodcells <- which(apply(monfrac, 3, function(v) {!all(is.na(v))}))
    ## Check that all the good cells have valid data and none of the bad cells
    ## do.  Also, there should be exactly 67420 good cells.
    if(test>0) {
        ## If testing then just take a few grid cells
        goodcells = goodcells[seq(1,test)]
    }
    else {
        ## These tests are only guaranteed to be valid if we're working with a
        ## full dataset.
        assert_that(length(goodcells) == 67420,
	            msg=paste("Expected 67420 good cells, found", length(goodcells)))
        assert_that(all(is.na(monfrac[ , , -goodcells])))
    }

    ## Clean up the data
    # 1. pull out the land cells
    monfrac_good <- monfrac[, , goodcells]     # year X month X grid 3d array


    # 2.  check that each land grid cell sums to 12(ish) in each year
    ann_grid_sums <- apply(monfrac_good, c(1,3), sum)
    assert_that(length(which(abs(ann_grid_sums - 12) > 1e-5)) == 0,
                msg=paste("Expected every land grid cell to sum fractions to 12 in each year,
                          found", length(which(abs(ann_grid_sums - 12) > 1e-5)), "cells that did not"))
    rm(ann_grid_sums)


    # 3. too small fraction values can cause issues with the fit, will be replaced with a min val
    minval <- 1e-6
    monfrac_good[monfrac_good < minval] <- minval

    # 4. if any land cells have NAs, check to make sure it's
    # all 12 months in a year in a grid cell

    # calculate the number of NAs per year per grid
    if(any(is.na(monfrac_good))){
      numNA_peryear_pergrid <- apply(monfrac_good, c(1,3), function(x){
        return(length(which(is.na(x))))
      })

      # if you get a grid cell with only some months in a year = NA,
      # that isn't a 0 precip caused issue. Throw an error.
      if(length(which(numNA_peryear_pergrid  > 0 & numNA_peryear_pergrid < 12)) > 0) {

        stop('a land cell has NA data not caused by a 0 precip year,
         should not be the case for ISIMIP2b RCPs.')

      }else{ # The only NAs you have are full years - means a 0 precip year
        # replace the value in each month of that year with a value of 1,
        # so the annual fractions in that grid cell still sum up.
        monfrac_good[is.na(monfrac_good)] <- 1
      }
      rm(numNA_peryear_pergrid)
    }

    # Update the full set of fractions
    monfrac[ , , goodcells] <- monfrac_good
    rm(monfrac_good)


    ## Compile the stan model
    stanmod <- stan_model(stanfile)

    nyear <- dim(monfrac)[1]

    nbatch <- 10
    ntot <- length(goodcells)
    batchlen <- as.integer(ceiling(ntot / nbatch))
    alpha <- matrix(nrow=12, ncol=dim(monfrac)[3])
    for (ibatch in seq(0,nbatch-1)) {
        strt <- 1 + ibatch*batchlen
        stp <- pmin(strt + batchlen-1, ntot)
        batchcells <- goodcells[strt:stp]
        monfrac_batch <- monfrac[, , batchcells]
        nc <- dim(monfrac_batch)[3]

        alpha_fit <-
            foreach(igrid=1:nc, .packages='rstan') %dopar% {
                obs <- monfrac_batch[, , igrid]
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
        alpha_fit <- simplify2array(alpha_fit)
        alpha[ , batchcells] <- alpha_fit
        fn <- sprintf('ckpt%03d.rds', ibatch)
        saveRDS(alpha, file.path(outdir, fn))
        message('Finished batch ', ibatch+1, ' of ', nbatch, '.')
    }

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
procmodel <- function(modelidx, test=0, outdir='.', write_data=TRUE, nodefile=NULL, nproc=4)
### :param modelidx: Index into the modelnames array above
{
message('starting procmodel')
    model <- modelnames[modelidx]
    if(!is.null(nodefile)) {
        clust_setup(nodefile, nproc)
    }
    else {
        registerDoParallel(cores=nproc)
    }

    ## checkpoint dir
    ckptdir <- file.path(outdir, paste0('ckpt-',model))
    dir.create(ckptdir)

    ## Precipitation fractions
    pr_frac_file <- file.path('./output-L1', paste0('pr_', model,
                                                  '_monthlyFrac.nc'))
    fracdata_pr <- readnc(pr_frac_file, 'prAdjust')
    alpha_pr <- dirfit(fracdata_pr, ckptdir, test=test)

    ## Temperature fractions
    tas_frac_file <- file.path('./output-L1', paste0('tas_', model,
                                                   '_monthlyFrac.nc'))
    fracdata_tas <- readnc(tas_frac_file, 'tasAdjust')
    alpha_tas <- dirfit(fracdata_tas, ckptdir, test=test)

    ## Create coordinate structure
    ncfile <- file.path('./output-L1', paste0('pr_', model, '_monthlyFrac.nc'))
    coord <- nccoord(ncfile)

    ## The 'time' element of the final output
    mnames <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
                'August', 'September', 'October', 'November', 'December')
    mnum <- sprintf('%02d', seq_along(mnames))
    time <- tibble(month=mnum, month_name=mnames)

    outfile <- model
    outfile <- file.path('output-L3', paste0('alpha_', outfile, '.rds'))

    outdata <- list(tas=alpha_tas, pr=alpha_pr, coord=coord, time=time)

    if(write_data) {
        message('Output file is ', outfile)
        saveRDS(outdata, file=outfile)
    }
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

clust_setup <- function(nodefile, nproc)
{
    nodes <- readr::read_lines(nodefile)
    ncore <- ceiling(nproc / length(nodes)) # cores per node
    ## Each node has to appear in the list a number of times equal to the number
    ## of processes you want to run on it.
    nodes <- rep(nodes, ncore)

    cl <- makeCluster(nodes, outfile='')
    registerDoParallel(cl)

}
message('sourced function script')
