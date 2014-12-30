library(dplyr)
library(tidyr)
library(lubridate)
library(RPostgreSQL)
library(ggplot2)

# connect to database source
db <- src_postgres(dbname='conte_dev', host='127.0.0.1', port='5432', user='conte', password='conte')

# table references
tbl_locations <- tbl(db, 'locations') %>%
  rename(location_id=id, location_name=name, location_description=description) %>%
  select(-created_at, -updated_at)
tbl_agencies <- tbl(db, 'agencies') %>%
  rename(agency_id=id, agency_name=name) %>%
  select(-created_at, -updated_at)
tbl_series <- tbl(db, 'series') %>%
  rename(series_id=id) %>%
  select(-created_at, -updated_at)
tbl_variables <- tbl(db, 'variables') %>%
  rename(variable_id=id, variable_name=name, variable_description=description) %>%
  select(-created_at, -updated_at)
tbl_values <- tbl(db, 'values') %>%
  rename(value_id=id)
tbl_daymet <- tbl(db, 'daymet')
tbl_covariates <- tbl(db, 'covariates')

# list of agencies to keep
keep_agencies <- c('MADEP', 'MAUSGS')

# fetch locations
df_locations <- left_join(tbl_locations, tbl_agencies, by=c('agency_id'='agency_id')) %>%
  filter(agency_name %in% keep_agencies) %>%
  rename(featureid=catchment_id) %>%
  collect
summary(df_locations)
unique(df_locations$agency_name)

# fetch covariates
df_covariates <- filter(tbl_covariates, featureid %in% df_locations$featureid) %>%
  collect %>%
  spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

# fetch temperature data
df_values <- left_join(tbl_series,
                       select(tbl_variables, variable_id, variable_name),
                       by=c('variable_id'='variable_id')) %>%
  select(-file_id) %>%
  filter(location_id %in% df_locations$location_id,
         variable_name=="TEMP") %>%
  left_join(tbl_values,
            by=c('series_id'='series_id')) %>%
  left_join(select(tbl_locations, location_id, location_name, latitude, longitude, featureid=catchment_id),
            by=c('location_id'='location_id')) %>%
  left_join(tbl_agencies,
            by=c('agency_id'='agency_id')) %>%
  collect %>%
  mutate(datetime=with_tz(datetime, tzone='EST'))
summary(df_values)

# create temperatureData input dataset
temperatureData <- select(df_values, location_id, agency_name, location_name, latitude, longitude, featureid, variable_name, datetime, value, flagged) %>%
  mutate(agency_name=factor(agency_name),
         location_name=factor(location_name),
         variable_name=factor(variable_name))
summary(temperatureData)

# create covariateData input dataset
covariateData <- left_join(select(df_locations, location_id, location_name, latitude, longitude, featureid),
                           df_covariates,
                           by=c('featureid'='featureid')) %>%
  mutate(location_name=factor(location_name))
summary(covariateData)

# create climateData input dataset
climate <- tbl_daymet %>%
  filter(featureid %in% df_locations$featureid)

climateData <- collect(climate)

saveRDS(temperatureData, file='temperatureData.RData')
saveRDS(covariateData, file='covariateData.RData')
saveRDS(climateData, file='climateData.RData')

filter(temperatureData, location_name %in% unique(temperatureData$location_name)[1:6]) %>%
  ggplot(aes(datetime, value)) +
  geom_point() +
  facet_wrap(~location_name)