# Validate model
# requires coef input binary file (coef.list list of dataframes), the covariate_list binary file (cov.list list of character strings) and tempDataSync file
# saves output rmse.table to binary file rmse_table.RData
#
# usage: $ Rscript validate_model.R <input tempDataSync rdata> <input covariate_list rdata> <input coef rdata> <output rmse_table rdata>
# example: $ Rscript validate_model.R ./tempDataSync.RData ./covariate_list.RData ./coef.RData ./rmse_table.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

tempDataSync_file <- args[1]
if (!file.exists(tempDataSync_file)) {
  stop(paste0('Could not find tempDataSync binary file: ', tempDataSync_file))
}
load(tempDataSync_file)

cov_file <- args[2]
if (!file.exists(cov_file)) {
  stop(paste0('Could not find covariate binary file: ', cov_file))
}
cov.list <- readRDS(cov_file)

coef_file <- args[3]
if (!file.exists(coef_file)) {
  stop(paste0('Could not find covariate binary file: ', coef_file))
}
coef.list <- readRDS(coef_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

# ----
library(ggplot2)
library(dplyr)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(rjags)

tempDataSyncS$tempPredicted <- NA
tempDataSyncS$tempPredicted <- predictTemp(data = tempDataSyncS, coef.list = coef.list, cov.list = cov.list, firstObsRows = firstObsRows, evalRows = evalRows)

tempDataSyncValidS$tempPredicted <- NA
tempDataSyncValidS$tempPredicted <- predictTemp(data = tempDataSyncValidS, firstObsRows = firstObsRows.valid, evalRows = evalRows.valid, coef.list = coef.list, cov.list = cov.list)

#library(ggplot2)
#ggplot(tempDataSyncValidS, aes(temp, tempPredicted)) + geom_point() + geom_abline(aes(1,1), colour = 'blue')
tempDataSyncValidS$resid.r <- tempDataSyncValidS$temp - tempDataSyncValidS$tempPredicted
rmse.valid <- rmse(tempDataSyncValidS$resid.r)


#library(ggplot2)
#ggplot(tempDataSyncS, aes(temp, tempPredicted)) + geom_point() + geom_abline(aes(1,1), colour = 'blue')

tempDataSyncS$resid.r <- tempDataSyncS$temp - tempDataSyncS$tempPredicted

rmse.fit <- rmse(tempDataSyncS$resid.r)
rmse.table <- data.frame(rbind(rmse.fit, rmse.valid))

saveRDS(rmse.table, file=output_file)

