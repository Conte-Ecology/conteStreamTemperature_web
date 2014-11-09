library(dplyr)
library(stringr)

BASE_DIR <- '../dataIn'
OUT_DIR <- 'out'
if (!file.exists(OUT_DIR)) {
  dir.create(OUT_DIR)
}

agencies <- c('CTDEP','MAFW','MAUSGS','MADEP' ,'NHFG','NHDES','USFS','VTFWS','MEDMR')

sapply(agencies, function(agency) {
  cat('AGENCY:', agency)
  agency_dir <- file.path(BASE_DIR, agency)
  if (!file.exists(agency_dir)) {
    stop(paste0('Could not find agency directory: ', agency_dir))
  }
  
  agency_file_temp <- file.path(agency_dir, paste0('observedStreamTempAndClimateData_', agency, '.RData'))
  if (!file.exists(agency_file_temp)) {
    stop(paste0('Could not find file: ', agency_file_temp))
  }
  
  agency_file_cov <- file.path(agency_dir, paste0('covariateData_', agency, '.RData'))
  if (!file.exists(agency_file_cov)) {
    stop(paste0('Could not find file: ', agency_file_cov))
  }
  
  load(agency_file_temp)
  load(agency_file_cov)
  
  # remove agency prefix from site names
  masterData <- mutate(masterData, site=str_replace(site, paste0(agency, '_'), ''))
  catchments <- select(covariateData, catchment_id=FEATUREID, site) %>%
    mutate(site=str_replace(site, paste0(agency, '_'), ''))
  
  tempData <- select(masterData, site, datetime=date, value=temp) %>%
    # exclude 
    filter(!is.na(value))
  
  siteData <- select(masterData, agency, site, latitude=Latitude, longitude=Longitude) %>%
    # extract unique rows
    unique
  
  # check for duplicate sites having more then one unique location
  siteData.duplicates <- group_by(siteData, site) %>%
    summarise(N=n()) %>%
    filter(N>1)
  if (nrow(siteData.duplicates) > 0) {
    stop(paste0('Duplicate sites for ', agency, ': ', paste(siteData.duplicates$site, collapse=', ')))
  }
  
  # check for duplicate sites having more then one unique catchment
  catchments.duplicates <- group_by(catchments, site) %>%
    summarise(N=n()) %>%
    filter(N>1)
  if (nrow(catchments.duplicates) > 0) {
    stop(paste0('Duplicate catchments for ', agency, ': ', paste(catchments.duplicates$site, collapse=', ')))
  }
  
  # add catchment_id to siteData
  siteData <- left_join(siteData, catchments)
  
  if (any(is.na(siteData$catchment_id))) {
    stop(paste0('Missing catchments for ', agency, ' sites: ', sum(is.na(siteData$catchment_id))))
  }
  
  # save to csv
  if (!file.exists(file.path(OUT_DIR, agency))) {
    dir.create(file.path(OUT_DIR, agency))
  }
  write.csv(siteData, file=file.path(OUT_DIR, agency, 'locations.csv'), row.names=FALSE)
  write.csv(tempData, file=file.path(OUT_DIR, agency, 'values.csv'), row.names=FALSE)
  
  return(TRUE)
})
