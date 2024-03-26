#' process_population
#'
#' Process population and output standard population structure given the spatial scale
#'
#' @param population Default = NULL. String for path to population files (NetCDF or CSV). The CSV file need to have columns latitude, longitude, and years. For example,  [latitude, longitude, 2020, 2021, ...]
#' @param coordinates Default = NULL. Data frame of latitudes and longitudes to map the population. Columns need to be [lat, lon]
#' @param time_periods Default = NULL. Integer vector for selected time periods to process. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @param climate_years Default = NULL. Integer vector of years that is covered by the climate data to check if the population years overlap with the climate years. If Null, skip the check.
#' @param spatial Default = NULL. String for spatial aggregation boundaries. Options: check helios::spatial_options. 'gcam_us49', 'gcam_regions32', 'gcam_regions31_us52', 'gcam_countries', 'gcam_basins'.
#' @param im3_analysis Default = TRUE. Output annual HDCD at grid region scale for trend-representative year analysis
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

process_population <- function(population = NULL,
                               coordinates = NULL,
                               time_periods = NULL,
                               climate_years = NULL,
                               spatial = NULL,
                               im3_analysis = TRUE){

  if (im3_analysis) {

    # use the population at each time period for the HDCD years within that period
    # (1) year 2020 for period 2020
    # (2) year 2021 - 2025 for period 2025, ..., year 2091-2095 for period 2095
    # (3) year 2096 - 2099 for period 2100
    # get the trend-representative year for each 5-year period
    periods <- c(as.Date("2020-01-01"),
                 as.Date("2021-01-01"),
                 seq(as.Date("2026-01-01"), as.Date("2101-01-01"), by= "5 years"))

    mapping_time_period <- data.frame(year = time_periods) %>%
      dplyr::mutate(date = as.Date(paste0(year, '-01-01')),
                    period = cut(date, breaks = periods, right = FALSE, labels = periods[2:length(periods)]),
                    period = lubridate::year(as.Date(period) - 1)) %>%
      dplyr::select(-date)

    # read population based on the population data type
    # output from wrf resolution: ['ID', 'subRegion', 'lat', 'lon', 'year', 'value' ]
    # output from 1/8th degree pop data: ['ID', 'subRegion', 'lat', 'lon', 'year', 'value' ]
    population_grid <- helios::read_population(file = population,
                                               time_periods = unique(mapping_time_period$period))

    # expand population data from 5-year period to annual based on the mapping_time_period
    # the annual population for years within the period will be the same as the population for the period
    # E.g., population from years 2021 - 2025 uses population in period 2025
    population_grid <- population_grid %>%
      tidyr::expand(tidyr::nesting(lat, lon),
                    year = time_periods) %>%
      dplyr::left_join(mapping_time_period, by = 'year') %>%
      dplyr::left_join(population_grid, by = c('lat', 'lon', 'period' = 'year')) %>%
      dplyr::select(-period)

  } else {

    # read population based on the population data type
    # output from wrf resolution: ['ID', 'subRegion', 'lat', 'lon', 'year', 'value' ]
    # output from 1/8th degree pop data: ['ID', 'subRegion', 'lat', 'lon', 'year', 'value' ]
    population_grid <- helios::read_population(file = population_j,
                                               time_periods = time_periods)

  }

  population_grid <- helios::match_grids(from_df = population_grid,
                                         to_df = coordinates,
                                         time_periods = time_periods)

  grid_intersect <- coordinates %>%
    dplyr::select(lon, lat) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(population_grid %>% dplyr::select(lon, lat),
                      by = c('lon', 'lat'))
  # check if population's grid matches climate data's grids
  if(nrow(grid_intersect) == 0) {
    stop('Climate and population data has different resolutions.')
  }

  population_grid <- helios::find_mapping_grid(data = population_grid,
                                               spatial = spatial)


  # Create population weighted raster if any population years in ncdf years
  if (is.null(climate_years) | any(unique(population_grid$year) %in% climate_years)) {

    # Weighted population tibble
    print('Starting population weighting ...')

    population_weighted <- population_grid %>%
      dplyr::group_by(region, ID, subRegion, year) %>%
      dplyr::mutate(subRegion_total_value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pop_weight = value / subRegion_total_value)

    out_list <- list(population_weighted = population_weighted)

    # for IM3 grid region analysis
    if(im3_analysis){
      population_weighted_gridregion <- population_grid %>%
        dplyr::left_join(helios::mapping_states_gridregion, by = 'subRegion') %>%
        dplyr::mutate(subRegion = grid_region,
                      ID = grid_region) %>%
        dplyr::select(-grid_region) %>%
        dplyr::group_by(region, ID, subRegion, year) %>%
        dplyr::mutate(subRegion_total_value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(pop_weight = value / subRegion_total_value)

      out_list <- list(population_weighted = population_weighted,
                       population_weighted_gridregion = population_weighted_gridregion)
    }

    print('Completed population weighting.')

  } else {
    print(paste0('Population data years: ', paste(names(population_grid)[!grepl('RID|lat|lon', names(population_grid))], collapse = ',')))
    print(paste0('ncdf data years: ', as.character(climate_years)))
    stop('Population data provided does not contain data for any of the years in the ncdf data.')
  }


  return(out_list)
}
