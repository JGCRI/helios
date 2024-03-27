#' process_temperature
#'
#' Process temperature netCDF and output standard HDCD in degree F
#'
#' @param ncdf Default = NULL. String or vector for paths to the NetCDF or CSV file. Or data table with the same output format from the process_temperature.
#' @param ncdf_var Default = NULL. String for variable name to extract from NetCDF file. Temperature var is 'tas' for CMIP models; 'T2' for WRF model.
#' @param model Default = NULL. String for climate model that generates the ncdf file. Options: 'wrf' or 'cmip'.
#' @param spatial Default = NULL. String for spatial aggregation boundaries. Options: check helios::spatial_options. 'gcam_us49', 'gcam_regions32', 'gcam_regions31_us52', 'gcam_countries', 'gcam_basins'.
#' @param time_periods Default = NULL. Integer vector for selected time periods to process. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @param reference_temp_F Default = 65. Integer for comfort temperature in degree F. 65 degree F is the comfort baseline temperature typically used by NOAA. The comfort temperature can vary by regions.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

process_temperature <- function(ncdf = NULL,
                                ncdf_var = NULL,
                                model = NULL,
                                spatial = NULL,
                                time_periods = NULL,
                                reference_temp_F = 65){

  if (is.data.frame(ncdf)){

     ncdf_grid <- ncdf

  } else {

    file_ext <- strsplit(basename(ncdf), '\\.')[[1]][-1]

    if(file_ext == 'nc'){

      # read ncdf file
      ncdf_grid <- helios::read_ncdf(ncdf = ncdf,
                                     model = model,
                                     var = ncdf_var,
                                     time_periods = time_periods)

      # find region and subRegion info based on data grid lat lon
      ncdf_grid <- helios::find_mapping_grid(data = ncdf_grid,
                                             spatial = spatial)

      # calculate heating and cooling degrees
      ncdf_grid <- ncdf_grid %>%
        dplyr::mutate(across(c(-lat, -lon, -region, -subRegion, -ID),
                             ~ round((((. - 273.15) * 9/5) + 32) - reference_temp_F, 2)))

    } else if (file_ext == 'csv') {

      ncdf_grid <- data.table::fread(file = ncdf)

    }

  }


  return(ncdf_grid)

}
