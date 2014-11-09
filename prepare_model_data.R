# Prepare model input dataset (tempDataSync)
# requires masterData, covariateData input binary files
# saves output springFallBPs to binary file
#
# usage: $ Rscript prepare_model_data.R <input masterData rdata> <input covariateData rdata> <input springFallBPs rdata> <output tempDataSync rdata>
# example: $ Rscript prepare_model_data.R ./masterData.RData ./covariateData.RData ./springFallBPs.RData ./tempDataSync.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

library(devtools)

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
masterData_file <- args[1]
if (!file.exists(masterData_file)) {
  stop(paste0('Could not find masterData binary file: ', masterData_file))
}
masterData <- readRDS(masterData_file)

covariateData_file <- args[2]
if (!file.exists(covariateData_file)) {
  stop(paste0('Could not find covariateData binary file: ', covariateData_file))
}
covariateData <- readRDS(covariateData_file)

springFallBPs_file <- args[3]
if (!file.exists(springFallBPs_file)) {
  stop(paste0('Could not find springFallBPs binary file: ', springFallBPs_file))
}
covariateData <- readRDS(covariateData_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

## First choose the agency data that will be analyzed
# load(paste0(dataOutDir, 'springFallBreakpoints.RData'))

# ----
# # r Choose data source that will be analyzed
# 
# #Northeast
# CTDEP  <- F
# MAFW   <- T
# MAUSGS <- T
# MADEP  <- T 
# NHFG   <- F
# NHDES  <- F
# USFS   <- F
# VTFWS  <- F
# MEDMR  <- F
# 
# #Montana
# MTUSGSYellowstone <- F
# MTUSGSGlacier <- F
# 
# sourceChoice <- list( CTDEP,   MAFW,   MAUSGS, MADEP,   NHFG,   NHDES,   MEDMR,   USFS,   VTFWS,    MTUSGSYellowstone,   MTUSGSGlacier)
# sourceNames  <- c   ('CTDEP', 'MAFW', 'MAUSGS', 'MADEP', 'NHFG', 'NHDES', 'MEDMR', 'USFS', 'VTFWS',  'MTUSGSYellowstone', 'MTUSGSGlacier')
# 
# dataSource <- sourceNames[sourceChoice == T]
# 
# fields <- c("agency", "date", "AgencyID", "year", "site", "date", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")
# 
# Read the data from those agencies and join with breakpoint data then filter (clip) to the syncronized portion of the year.
# 
# tempData <- readStreamTempData(timeSeries=TRUE, covariates=TRUE, dataSourceList=dataSource, fieldListTS=fields, fieldListCD='ALL', directory=dataInDir)

masterData <- masterData[, c("agency", "date", "AgencyID", "year", "site", "date", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")]
tempData <- left_join(tempData, covariateData)

str(tempData) # show the structure

springFallBPs$site <- as.character(springFallBPs$site)

# Join with break points
tempDataBP <- left_join(tempData, springFallBPs, by=c('site', 'year'))
rm(tempData) # save some memory

# Clip to syncronized season
tempDataSync <- filter(tempDataBP, dOY >= finalSpringBP & dOY <= finalFallBP)

# Creat site and year factor variables
tempDataSync$fyear <- as.factor(tempDataSync$year)
tempDataSync$fsite <- as.factor(tempDataSync$site)

# Head and structure for Jeff

# head(tempDataSync)
# str(tempDataSync)

# Create lagged air temperature and precipitation

# Order by group and date
tempDataSync <- tempDataSync[order(tempDataSync$site,tempDataSync$year,tempDataSync$dOY),]

# For checking the order of tempDataSync
tempDataSync$count <- 1:length(tempDataSync$year)

tempDataSync <- tempDataSync[order(tempDataSync$count),] # just to make sure tempDataSync is ordered for the slide function

# airTemp
tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -1, NewVar='airTempLagged1')
tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -2, NewVar='airTempLagged2')

# prcp
tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -1, NewVar='prcpLagged1')
tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -2, NewVar='prcpLagged2')
tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -3, NewVar='prcpLagged3')

# Select the data potentially wanted for the analysis

# Make dataframe with just variables for modeling and order before standardizing
tempDataSync <- tempDataSync[ , c("agency", "date", "AgencyID", "year", "fyear", "site", "fsite", "date", "finalSpringBP", "finalFallBP", "FEATUREID", "HUC4", "HUC8", "HUC12", "temp", "Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")] #  

summary(tempDataSync)
dim(tempDataSync)
tempDataSync <- na.omit(tempDataSync) ####### Change this so don't take out NA in stream temperature
dim(tempDataSync)

### Check variables for correlation

# check correlation among potential independent variables
# Cannot plot all points because will overload the plot and lock the system up - therefore thin first
pairs.full <- data.frame(lat = tempDataSync$Latitude,
                         lon = tempDataSync$Longitude,
                         airTemp = tempDataSync$airTemp, 
                         precip = tempDataSync$prcp,
                         drainage = tempDataSync$TotDASqKM,
                         forest = tempDataSync$Forest,
                         elevation = tempDataSync$ReachElevationM,
                         coarseness = tempDataSync$SurficialCoarseC,
                         wetland = tempDataSync$CONUSWetland,
                         impoundments = tempDataSync$ImpoundmentsAllSqKM,
                         swe = tempDataSync$swe,
                         dOY = tempDataSync$dOY, 
                         dOY2 = tempDataSync$dOY^2)

pairs.thin <- sample_n(pairs.full, 3000, replace = F)

# Move these into the package as helper functions--------
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex) # * abs(r) # change color to red if >0.7
}
#--------

# pairs(pairs.thin, upper.panel=panel.smooth, lower.panel=panel.cor, diag.panel=panel.hist)

# impoundments and drainage have some outliers. Look in more detail and reduce dataset to reflect this so inference is not based on a few sites in large drainages with dozens of impoundments. Could also check to make sure these are properly classified and not small tribuaries that get place in the main river (CT) catchment.
# hist(tempDataSync$TotDASqKM)
# dim(tempDataSync)
# length(unique(tempDataSync$site))

tempDataSync <- filter(tempDataSync, filter = TotDASqKM <= 200)
# hist(tempDataSync$TotDASqKM)
# dim(tempDataSync)
# length(unique(tempDataSync$site))

# **Inference only on catchments with total drainage area <= 200 km^2

# No problems of correlation among these potential independent covariates

### Separate data for fitting (training) and validation

#Use validation?
validate = T

#If validating:
# Choose fraction of total # of sites:
validateFrac <- 0.2

if(validate) {
  n.fit <- floor(length(unique(tempDataSync$site)) * (1 - validateFrac))
  
  set.seed(2346)
  site.fit <- sample(unique(tempDataSync$site), n.fit, replace = FALSE) # select sites to hold back for testing 
  tempDataSyncValid <- subset(tempDataSync, !site %in% site.fit) # data for validation
  tempDataSync <- subset(tempDataSync, site %in% site.fit)    # data for model fitting (calibration)
} else {
  tempDataSyncValid <- NULL
}

### Standardize continuous covariates for analysis

# Standardize for Analysis

tempDataSyncS <- cbind(tempDataSync[ ,c(1:15)],
                       apply(X = tempDataSync[ ,16:dim(tempDataSync)[2]], MARGIN=2,
                             FUN = function(x){(x-mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)}))



tempDataSyncValidS <- cbind(tempDataSyncValid[ ,c(1:15)],
                            apply(X = tempDataSyncValid[ ,16:dim(tempDataSyncValid)[2]], MARGIN=2,
                                  FUN = function(x){(x-mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)}))

tempDataSyncValidS$fyear <- as.factor(tempDataSyncValidS$year)
tempDataSyncValidS$fsite <- as.factor(tempDataSyncValidS$site)

### Save data (for backtransformation later), standardized data (for analysis) from both the calibration and validation datasets

save(tempDataSync, tempDataSyncS, tempDataSyncValid, tempDataSyncValidS, file=output_file)

# Head and structure for Jeff. The calibration and validation structures are the same just subsets of the same data frames.

# head(tempDataSync)
# str(tempDataSync)
# str(tempDataSyncS)
