# -----------------------------------------------------------
# Initialization
# -----------------------------------------------------------
# if using R poject
devtools::load_all()

library(helios)
library(dplyr)


# -----------------------------------------------------------
# Test the small data set
# -----------------------------------------------------------

# WRF data -----------------

# example data: spatial subset (few grid cells within USA) of WRF hourly climate at 12-km resolution, covers 7 days
path_to_climate_ncdf <- helios::pkg_example('wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc')

# example data: population of 2020 at same 12-km resolution as WRF
path_to_population <- helios::pkg_example('population_conus_ssp2_2020wrf_wgs84.csv')

# read ncdf data
ncdf_grid <- helios::read_ncdf(ncdf = path_to_climate_ncdf,
                               model = 'wrf',
                               var = 'T2',
                               time_periods = 2020)

pop <- helios::read_population(path_to_population, time_periods = 2020)

# check mapping grid
mapping <- helios::find_mapping_grid(data = pop,
                                     spatial = 'gcam_regions31_us52')

# Calculate heating and coolong degrees for CONUS (e.g., part of USA in the example)
hdcd_wrf <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = 'T2',
  model = 'wrf',
  model_timestep = 'hourly',
  population = path_to_population,
  spatial = 'gcam_us49',
  time_periods = 2020,
  dispatch_segment = T,
  reference_temp_F = 65,
  folder = file.path(getwd(), 'output'),
  diagnostics = T,
  xml = T,
  name_append = '',
  save = T
)


helios::save_xml(hdcd_gcam = hdcd_wrf$hdcd_comb_gcam,
                 folder = file.path(getwd(), 'output'))

# # Test when spatial is a data frame with subRegions
# spatial <- data.frame(subRegion = c("CA","FL","ID","MO","TX","WY"))
#
# hdcd_wrf <- helios::hdcd(
#   ncdf = path_to_climate_ncdf,
#   ncdf_var = 'T2',
#   model = 'wrf',
#   model_timestep = 'hourly',
#   population = path_to_population,
#   spatial = spatial,
#   time_periods = 2020,
#   dispatch_segment = T,
#   reference_temp_F = 65,
#   folder = file.path(getwd(), 'output'),
#   diagnostics = F,
#   xml = F,
#   name_append = '',
#   save = F
# )

# CMIP data ----------------

# example data: spatial subset (gew grid cells within China) CMIP6 daily climate at 0.5 degree resolution from 2015 to 2020
path_to_climate_ncdf <- helios::pkg_example('gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc')

# example data: population of 2020 at 0.125 degree resolution
path_to_population <- helios::pkg_example('ssp1_2020_sub.nc')

# read ncdf data
ncdf_grid <- helios::read_ncdf(ncdf = path_to_climate_ncdf,
                               model = 'cmip',
                               var = 'tas',
                               time_periods = 2020)

pop <- helios::read_population(path_to_population, time_periods = 2020)

# try match grids from pop to ncdf_grid
pop_match <- helios::match_grids(from_df = pop,
                                 to_df = ncdf_grid,
                                 time_periods = 2020)

# check mapping grid
mapping <- helios::find_mapping_grid(data = pop,
                                     spatial = 'gcam_regions31_us52')

# Calculate heating and coolong degrees for GCAM regions (e.g., part of China in the example)
hdcd_cmip <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = 'tas',
  model = 'cmip',
  model_timestep = 'daily',
  population = path_to_population,
  spatial = 'gcam_regions32',
  time_periods = 2020,
  dispatch_segment = F,
  reference_temp_F = 65,
  folder = file.path(getwd(), 'output'),
  diagnostics = T,
  xml = T,
  name_append = '',
  save = T
)

helios::save_xml(hdcd_gcam = hdcd_cmip$hdcd_comb_gcam,
                 folder = file.path(getwd(), 'output'))

# -----------------------------------------------------------
# Test the large data set
# -----------------------------------------------------------
# These data are not from the package
# Download data folder from Constance /pic/projects/im3/gcamusa/climateimpacts/helios/data
data_dir <- 'C:/WorkSpace/IM3/helios/example_data'

# WRF ---------------------
# CONUS WRF data
path_to_climate_ncdf <- file.path(data_dir, 'climate', 'wrfout_d01_2020-01-01_01%3A00%3A00.nc')
path_to_population <- file.path(
  data_dir, 'population', 'population_conus_total_ssp2_2020-2100_wrf_wgs84.csv')

hdcd_wrf_all <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = 'T2',
  model = 'wrf',
  model_timestep = 'hourly',
  population = path_to_population,
  spatial = 'gcam_us49',
  time_periods = 2020,
  dispatch_segment = T,
  reference_temp_F = 65,
  folder = file.path(getwd(), 'output_im3_test'),
  diagnostics = F,
  xml = F,
  name_append = '',
  save = T,
  im3_analysis = T
)

helios::diagnostics(
  hdcd_segment = hdcd_wrf_all$hdcd_comb_gcam,
  hdcd_monthly = hdcd_wrf_all$hdcd_comb_monthly,
  min_diagnostic_months = 1,
  folder = file.path(getwd(), 'output'),
  name_append = 'wrf_all'
)


# CMIP data ----------------
# Global CMIP6 data
# These data are not from the package
# Download data folder from Constance /pic/projects/im3/gcamusa/climateimpacts/helios/data

path_to_climate_ncdf <- file.path(data_dir, 'climate', 'gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc')
path_to_population <- file.path(data_dir, 'population', 'ssp1_2020.nc')

# path_to_climate_ncdf <- 'C:/WorkSpace/GCIMS/data/climate/isimip/isimip3b/cmip6/gfdl-esm4/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc'
# path_to_population <- 'C:/WorkSpace/IM3/helios/example_data/population/popdynamics-1-8th-pop-base-year-projection-ssp-2000-2100-rev01-proj-ssp5-netcdf/SSP5/Total/NetCDF/population_ssp5_interp.csv'

ncdf_grid <- helios::read_ncdf(ncdf = path_to_climate_ncdf,
                               model = 'cmip',
                               var = 'tas',
                               time_periods = 2020)
population_j_grid <- helios::read_population(path_to_population, time_periods = 2020)

hdcd_cmip_all <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = 'tas',
  model = 'cmip',
  model_timestep = 'daily',
  population = path_to_population,
  spatial = 'gcam_regions31_us52', # gcam_regions32
  time_periods = c(2015),
  dispatch_segment = F,
  reference_temp_F = 65,
  folder = paste0(getwd(), '/output'),
  diagnostics = F,
  xml = T,
  name_append = '',
  save = T
)

helios::diagnostics(
  hdcd_segment = hdcd_cmip_all$hdcd_comb_gcam,
  hdcd_monthly = hdcd_cmip_all$hdcd_comb_monthly,
  min_diagnostic_months = 6,
  folder = file.path(getwd(), 'output'),
  name_append = 'cmip_all'
)


# -----------------------------------------------------------
# Test diagnostic with WRF HDCD data from 2020 - 2050
# -----------------------------------------------------------

helios::diagnostics(
  hdcd_segment = helios::example_hdcd_segment_usa %>% dplyr::filter(year %in% seq(2020, 2050, 5)),
  hdcd_monthly = helios::example_hdcd_monthly_usa %>% dplyr::filter(year %in% seq(2020, 2050, 5)),
  min_diagnostic_months = 6,
  folder = file.path(getwd(), 'output'),
  name_append = 'wrf_rcp45cooler_ssp3'
)



# Test diagnostics with NERSC output on monthly HDHCDH at US49
hdhcdh_us49 <- data.table::fread(
  'C:/WorkSpace/IM3/helios/hddcdd/nersc/combined_outputs_hdcd_rcp45cooler_ssp3/combined_hdhcdh_2020_2099_monthly_us49.csv'
)

helios::diagnostics(
  hdcd_monthly = hdhcdh_us49,
  min_diagnostic_months = 6,
  folder = file.path(getwd(), 'output'),
  name_append = 'combined_hdhcdh_monthly_us49'
)

# Test diagnostics with NERSC output on monthly HDHCDH at Gridregion
hdhcdh_gridregion <- data.table::fread(
  'C:/WorkSpace/IM3/helios/hddcdd/nersc/combined_outputs_hdcd_rcp45cooler_ssp3/combined_hdhcdh_2020_2099_monthly_gridregion.csv'
)

helios::diagnostics(
  hdcd_monthly = hdhcdh_gridregion,
  min_diagnostic_months = 6,
  folder = file.path(getwd(), 'output'),
  name_append = 'combined_hdhcdh_monthly_gridregion'
)
