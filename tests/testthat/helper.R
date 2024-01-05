library(rmap)
#.......................
# Prepare data for tests
#.......................

# path to climate data example
wrf_usa_ncdf <- helios::pkg_example('wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc')
cmip6_china_ncdf <- helios::pkg_example('gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc')

pop_usa_csv <- helios::pkg_example('population_conus_ssp2_2020wrf_wgs84.csv')
pop_china_ncdf <- helios::pkg_example('ssp1_2020_sub.nc')


pop_test <- system.file(
  'extras',
  'noaa_hddcdd.png',
  package = 'helios')

run_hdcd_usa <- function(ncdf = wrf_usa_ncdf,
                         ncdf_var = 'T2',
                         model = 'wrf',
                         model_timestep = 'hourly',
                         population = pop_usa_csv,
                         spatial = 'gcam_us49',
                         time_periods = 2020,
                         dispatch_segment = T,
                         reference_temp_F = 65,
                         name_append = "",
                         diagnostics = F,
                         xml = F,
                         save = F){

  output <- helios::hdcd(ncdf = ncdf,
                         ncdf_var = ncdf_var,
                         model = model,
                         model_timestep = model_timestep,
                         population = population,
                         spatial = spatial,
                         time_periods = time_periods,
                         dispatch_segment = dispatch_segment,
                         reference_temp_F = reference_temp_F,
                         name_append = name_append,
                         diagnostics = diagnostics,
                         xml = xml,
                         save = save)

  return(output)
}

run_hdcd_china <- function(ncdf = cmip6_china_ncdf,
                           ncdf_var = 'tas',
                           model = 'cmip',
                           model_timestep = 'daily',
                           population = pop_china_ncdf,
                           spatial = 'gcam_regions32',
                           time_periods = 2020,
                           dispatch_segment = F,
                           reference_temp_F = 65,
                           name_append = "",
                           diagnostics = F,
                           xml = F,
                           save = F){

  output <- helios::hdcd(ncdf = ncdf,
                         ncdf_var = ncdf_var,
                         model = model,
                         model_timestep = model_timestep,
                         population = population,
                         spatial = spatial,
                         time_periods = time_periods,
                         dispatch_segment = dispatch_segment,
                         reference_temp_F = reference_temp_F,
                         name_append = name_append,
                         diagnostics = diagnostics,
                         xml = xml,
                         save = save)

  return(output)
}

run_diagnostics <- function(hdcd_segment = helios::example_hdcd_segment_usa %>%
                              dplyr::filter(year %in% c(2020, 2025)),
                            hdcd_monthly = helios::example_hdcd_monthly_usa %>%
                              dplyr::filter(year %in% c(2020, 2025)),
                            min_diagnostic_months = 6,
                            folder = paste0(getwd(), "/output"),
                            name_append = '') {

  helios::diagnostics(hdcd_segment = hdcd_segment,
                      hdcd_monthly = hdcd_monthly,
                      min_diagnostic_months = min_diagnostic_months,
                      folder = folder,
                      name_append = name_append)

}



