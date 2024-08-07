% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hdcd.R
\name{hdcd}
\alias{hdcd}
\title{hdcd}
\usage{
hdcd(
  ncdf = NULL,
  ncdf_var = NULL,
  model = NULL,
  model_timestep = NULL,
  population = NULL,
  spatial = NULL,
  time_periods = NULL,
  dispatch_segment = FALSE,
  reference_temp_F = 65,
  folder = file.path(getwd(), "output"),
  diagnostics = F,
  xml = F,
  name_append = "",
  save = T
)
}
\arguments{
\item{ncdf}{Default = NULL. String for path to the NetCDF file.}

\item{ncdf_var}{Default = NULL. String for variable name to extract from NetCDF file. Temperature var is 'tas' for CMIP models; 'T2' for WRF model.}

\item{model}{Default = NULL. String for climate model that generates the ncdf file. Options: 'wrf' or 'cmip'.}

\item{model_timestep}{Default = NULL. String for time step of input climate data. Options: 'hourly' or 'daily'}

\item{population}{Default = NULL. String for path to population files (NetCDF or CSV). The CSV file need to have columns latitude, longitude, and years. For example,  [latitude, longitude, 2020, 2021, ...]}

\item{spatial}{Default = NULL. String for spatial aggregation boundaries. Options: check helios::spatial_options. 'gcam_us49', 'gcam_regions32', 'gcam_regions31_us52', 'gcam_countries', 'gcam_basins'.}

\item{time_periods}{Default = NULL. Integer vector for selected time periods to process. If not specified, set to GCAM periods seq(2020, 2100, 5).}

\item{dispatch_segment}{Default = FALSE. Set to TRUE to output degree-hours by GCAM-USA dispatch segment. This can only be TRUE when model time_step is set to 'hourly'.}

\item{reference_temp_F}{Default = 65. Integer for comfort temperature in degree F. 65 degree F is the comfort baseline temperature typically used by NOAA. The comfort temperature can vary by regions.}

\item{folder}{Default = paste0(getwd(),'/output'). String for output folder path.}

\item{diagnostics}{Default = FALSE. Set to TRUE to create diagnostic figures.}

\item{xml}{Default = FALSE. Set to TRUE to generate XML outputs for GCAM.}

\item{name_append}{Default = ''. String for the name to append to output file name.}

\item{save}{Default = TRUE. Set to TRUE to save outputs.}
}
\description{
Heating and Cooling Degree processing for GCAM from various sources such as WRF and CMIP
}
