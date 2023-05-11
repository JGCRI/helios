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

# example data: WRF hourly climate at 12-km resolution
path_to_climate_ncdf <- helios::pkg_example("wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc")

# example data: population of 2020 at same 12-km resolution as WRF
path_to_population <- helios::pkg_example("population_conus_ssp2_2020wrf_wgs84.csv")

# Calculate heating and coolong degrees for CONUS (e.g., part of USA in the example)
hdcd_usa <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = "T2",
  model = "wrf",
  population = path_to_population,
  spatial = "states_us_49",
  temporal = 2020,
  reference_temp_F = 65,
  folder = paste0(getwd(), "/output"),
  diagnostics = T,
  xml = T,
  name_append = "",
  save = T
)


# CMIP data ----------------

# example data: CMIP6 daily climate at 0.5 degree resolution
path_to_climate_ncdf <- helios::pkg_example("gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc")

# example data: population of 2020 at 0.125 degree resolution
path_to_population <- helios::pkg_example("ssp1_2020_sub.nc")

# Calculate heating and coolong degrees for GCAM regions (e.g., part of China in the example)
hdcd_china <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = "tas",
  model = "cmip",
  population = path_to_population,
  spatial = "gcam_region_32",
  temporal = 2020,
  reference_temp_F = 65,
  folder = paste0(getwd(), "/output"),
  diagnostics = T,
  xml = T,
  name_append = "",
  save = T
)

helios::diagnostics(
  hdcd_monthly = hdcd_china$hdcd_comb_monthly,
  min_diagnostic_months = 6,
  folder = paste0(getwd(), "/output"),
  name_append = "cmip"
)



# -----------------------------------------------------------
# Test the large data set
# -----------------------------------------------------------

# WRF ---------------------
# These data are not from the package
# Download data folder from Constance /pic/projects/im3/gcamusa/climateimpacts/helios/data
data_dir <- "/pic/projects/im3/gcamusa/climateimpacts/helios/data"

path_to_climate_ncdf <- file.path(data_dir, "climate", "wrfout_d01_2020-01-01_01%3A00%3A00.nc")
path_to_population <- file.path(
  data_dir, 'population', "population_conus_total_ssp2_2020-2100_wrf_wgs84.csv"
)

hdcd_usa_full <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = "T2",
  model = "wrf",
  population = path_to_population,
  spatial = "states_us_49",
  temporal = 2020,
  reference_temp_F = 65,
  folder = paste0(getwd(), "/output"),
  diagnostics = F,
  xml = F,
  name_append = "",
  save = F
)

helios::diagnostics(
  hdcd = hdcd_usa_full$hdcd_comb,
  hdcd_monthly = hdcd_usa_full$hdcd_comb_monthly,
  min_diagnostic_months = 1,
  folder = paste0(getwd(), "/output"),
  name_append = "wrf"
)


# CMIP data ----------------
# These data are not from the package
path_to_climate_ncdf <- file.path(data_dir, "climate", "gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc")
path_to_population <- file.path(
  data_dir, 'population', "ssp1_2020.nc"
)
# path_to_climate_ncdf <- "C:/WorkSpace/GCIMS/data/climate/isimip/isimip3b/cmip6/gfdl-esm4/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc"
# path_to_population <- "C:/WorkSpace/IM3/helios/example_nersc_data/popdynamics-1-8th-pop-base-year-projection-ssp-2000-2100-rev01-proj-ssp1-netcdf/SSP1/Total/NetCDF/ssp1_2020.nc"

hdcd_china_full <- helios::hdcd(
  ncdf = path_to_climate_ncdf,
  ncdf_var = "tas",
  model = "cmip",
  population = path_to_population,
  spatial = "gcam_region_32",
  temporal = 2020,
  reference_temp_F = 65,
  folder = paste0(getwd(), "/output"),
  diagnostics = F,
  xml = T,
  name_append = "",
  save = T
)

helios::diagnostics(
  hdcd = hdcd_china_full$hdcd_comb,
  hdcd_monthly = hdcd_china_full$hdcd_comb_monthly,
  min_diagnostic_months = 6,
  folder = paste0(getwd(), "/output"),
  name_append = "cmip_full"
)


# -----------------------------------------------------------
# Test diagnostic with WRF HDCD data from 2020 - 2100
# -----------------------------------------------------------

helios::diagnostics(
  hdcd = helios::example_hdcd_segment_usa %>% dplyr::filter(year %in% seq(2020, 2050, 5)),
  hdcd_monthly = helios::example_hdcd_monthly_usa %>% dplyr::filter(year %in% seq(2020, 2050, 5)),
  min_diagnostic_months = 6,
  folder = paste0(getwd(), "/output"),
  name_append = "wrf_rcp45cooler_ssp3"
)

