# ‘~/R/x86_64-pc-linux-gnu-library/4.0’
BASE_DIR <- here::here()
OUT_DIR <- file.path(BASE_DIR, 'output-L3'); dir.create(OUT_DIR, showWarnings = FALSE)
setwd(file.path(BASE_DIR, 'data-raw'))

source('/pic/projects/GCAM/Dorheim/an2month/data-raw/L3.fit_dirichlet_params.R')

### Make a vector of all the model and experiments to process. This
### assumes that all there is a file for all of the model and experiment
### combinations. FYI if one of the scenarios is missing the procmodel
### function will throw an error.
model <- c('GFDL-ESM2M', 'HadGEM2-ES', 'IPSL-CM5A-LR', 'MIROC5')
rcps <- c('rcp26', 'rcp45', 'rcp60', 'rcp85')


models <- rep(model, each =length(rcps))
rcps <- rep(rcps, length(model))
modelnames <- paste(models, rcps, sep = '_')


for(index in seq_along(modelnames)){

  message('processing \n', modelnames[[index]])
  procmodel(index, outdir = OUT_DIR)

}


