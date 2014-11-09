Conte Web System Workflow
=========================

This folder contains scripts and documents outlining the workflow for running the stream temperature model.

Inputs:

- `catchments`: shapefile of NHDplus catchments
- `covariateData`: dataset containing the covariate data for each catchment 
- `temperatureData`: dataset containing observed stream temperatures as provided by various agencies
- `climateData`: dataset containing the climate data for each catchment

Outputs:

- `mcmcOutput`: output from JAGS containing MCMC runs
- `modelSummary`: summary of model parameters
- `predictions`: timeseries of predicted temperature for each catchment


The first step is to join the observed temperature data to the catchments
