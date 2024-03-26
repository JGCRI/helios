#' process_temperature
#'
#' Process temperature netCDF and output standard temperature structure
#'
#' @param ncdf Default = NULL. String of path to the NetCDF file.
#' @param ncdf_var Default = NULL. String for variable name to extract from NetCDF file. Temperature var is 'tas' for CMIP models; 'T2' for WRF model.
#' @param model Default = NULL. String for climate model that generates the ncdf file. Options: 'wrf' or 'cmip'.
#' @param spatial Default = NULL. String for spatial aggregation boundaries. Options: check helios::spatial_options. 'gcam_us49', 'gcam_regions32', 'gcam_regions31_us52', 'gcam_countries', 'gcam_basins'.
#' @param time_periods Default = NULL. Integer vector for selected time periods to process. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

process_temperature <- function(ncdf = NULL,
                                ncdf_var = NULL,
                                model = NULL,
                                spatial = NULL,
                                time_periods = NULL){

  # read ncdf file
  ncdf_grid <- helios::read_ncdf(ncdf = ncdf,
                                 model = model,
                                 var = ncdf_var,
                                 time_periods = time_periods)

  # find region and subRegion info based on data grid lat lon
  ncdf_grid <- helios::find_mapping_grid(data = ncdf_grid,
                                         spatial = spatial)

  # get the actual datetime from the ncdf
  ncdf_times <- names(ncdf_grid)[
    !names(ncdf_grid) %in% c('lat', 'lon', 'region', 'subRegion', 'ID')]

  indices <- as.integer(grepl(paste0(time_periods, collapse = '|'), ncdf_times))
  index_subset <- c(1:length(ncdf_times)) * indices
  index_subset <- index_subset[!index_subset %in% 0]
  years <- unique(substr(ncdf_times, 1, 4))

  if (model == 'wrf') {

    ncdf_pivot <- ncdf_grid %>%
      tidyr::pivot_longer(cols = dplyr::all_of(ncdf_times), names_to = 'datetime') %>%
      dplyr::mutate(datetime = as.POSIXct(datetime,
                                          format = '%Y-%m-%d_%H:%M:%S',
                                          tz = 'UTC')) %>%
      dplyr::mutate(year = lubridate::year(datetime))

  } else if (model == 'cmip') {

    ncdf_pivot <- ncdf_grid %>%
      tidyr::pivot_longer(cols = dplyr::all_of(ncdf_times), names_to = 'datetime') %>%
      dplyr::mutate(datetime = as.POSIXct(datetime,
                                          format = '%Y-%m-%d',
                                          tz = 'UTC')) %>%
      dplyr::mutate(year = lubridate::year(datetime))

  }

  return(list(ncdf_pivot = ncdf_pivot,
              ncdf_years = years,
              index_subset = index_subset))

}
