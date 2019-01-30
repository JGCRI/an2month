## read dirichlet parameters from output directory and add them to package data

library('usethis')

dir <- 'output-L3'
alpha_gfdl_esm2m <- readRDS(file.path(dir, 'alpha_gfdl_esm2m.rds'))
alpha_ipsl_cm5a_lr <- readRDS(file.path(dir, 'alpha_ipsl_cm5a_lr.rds'))
alpha_miroc_esm_chem <- readRDS(file.path(dir, 'alpha_miroc_esm_chem.rds'))
alpha_noresm1_m <- readRDS(file.path(dir, 'alpha_noresm1_m.rds'))

use_data(alpha_gfdl_esm2m, overwrite=TRUE, compress='xz')
use_data(alpha_ipsl_cm5a_lr, overwrite=TRUE, compress='xz')
use_data(alpha_miroc_esm_chem, overwrite=TRUE, compress='xz')
use_data(alpha_noresm1_m, overwrite=TRUE, compress='xz')


