#' format_temperature
#'
#' Format gridded temperature data and pivot it longer
#' Replace the design year/representative year with time period year for each grid region (for IM3)
#'
#' @param ncdf_grid Default = NULL. output from process_temperature.
#' @param model Default = NULL. String for climate model that generates the ncdf file. Options: 'wrf' or 'cmip'.
#' @param time_periods Default = NULL. Integer vector for selected time periods to process. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @param to_year Default = NULL. Integer for renaming the time step the design year/representative year is for.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

format_temperature <- function(ncdf_grid = NULL,
                               model = NULL,
                               time_periods = NULL,
                               to_year = NULL) {


  # get the actual datetime from the ncdf
  ncdf_times <- names(ncdf_grid)[
    !names(ncdf_grid) %in% c('lat', 'lon', 'region', 'subRegion', 'ID')]

  indices <- as.integer(grepl(paste0(time_periods, collapse = '|'), ncdf_times))
  index_subset <- c(1:length(ncdf_times)) * indices
  index_subset <- index_subset[!index_subset %in% 0]
  years <- unique(substr(ncdf_times, 1, 4))

  # rename the columns with the to_time_step
  if(!is.null(to_year)){

    ncdf_times_update <- gsub(years, to_year, ncdf_times)

    ncdf_grid <- ncdf_grid %>%
      dplyr::rename_at(dplyr::vars(ncdf_times), ~ ncdf_times_update)

    ncdf_times <- ncdf_times_update

  }


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

  return(
    list(ncdf_pivot = ncdf_pivot,
         index_subset = index_subset,
         years = years
         )
  )

}
