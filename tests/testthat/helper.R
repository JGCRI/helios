#.......................
# Prepare data for tests
#.......................

# path to climate data example
wrf_usa_ncdf <- system.file(
  'extras',
  'wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc',
  package = 'helios')
cmip6_china_ncdf <- system.file(
  'extras',
  'gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc',
  package = 'helios')

# path to population data example
pop_usa_csv <- system.file(
  'extras',
  'population_conus_ssp2_2020wrf_wgs84.csv',
  package = 'helios')
pop_china_ncdf <- system.file(
  'extras',
  'ssp1_2020_sub.nc',
  package = 'helios')

pop_test <- system.file(
  'extras',
  'noaa_hddcdd.png',
  package = 'helios')

run_hdcd_usa <- function(ncdf = wrf_usa_ncdf,
                         ncdf_var = 'T2',
                         model = 'wrf',
                         population = pop_usa_csv,
                         spatial = 'states_us_49',
                         temporal = 2020,
                         reference_temp_F = 65,
                         name_append = "",
                         diagnostics = F,
                         xml = F,
                         save = F){

  output <- helios::hdcd(ncdf = ncdf,
                         ncdf_var = ncdf_var,
                         model = model,
                         population = population,
                         spatial = spatial,
                         temporal = temporal,
                         reference_temp_F = reference_temp_F,
                         name_append = "",
                         diagnostics = diagnostics,
                         xml = xml,
                         save = save)

  return(output)
}

run_hdcd_china <- function(ncdf = cmip6_china_ncdf,
                           ncdf_var = 'tas',
                           model = 'cmip',
                           population = pop_china_ncdf,
                           spatial = 'gcam_region_32',
                           temporal = 2020,
                           reference_temp_F = 65,
                           name_append = "",
                           diagnostics = F,
                           xml = F,
                           save = F){

  output <- helios::hdcd(ncdf = ncdf,
                         ncdf_var = ncdf_var,
                         model = model,
                         population = population,
                         spatial = spatial,
                         temporal = temporal,
                         reference_temp_F = reference_temp_F,
                         name_append = "",
                         diagnostics = diagnostics,
                         xml = xml,
                         save = save)

  return(output)
}


