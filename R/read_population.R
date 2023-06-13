#' read_population
#'
#' Process population file depends on the input
#'
#' @param file Default = NULL. Path to population file. NetCDF or CSV
#' @param time_periods Default = NULL. integer vector. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export


read_population <- function(file = NULL, time_periods = NULL) {

  NULL -> ID -> region -> subRegion -> lat -> lon -> across -> RID -> x -> y ->
    layer -> value

  # If isn't a dataframe check if file exists
  if (length(file) == 1) {
    if (any(class(file) == 'character')) {
      if (file.exists(file)) {
        file_type <- strsplit(basename(file), split='\\.')[[1]][2]

        # ------------------------------------------------
        # directly read corresponding wrd population
        # ------------------------------------------------

        if (any(file_type %in% 'csv')) {

          file_raw <- data.table::fread(file)

          # Rename latitude and longitude if needed
          if (!any(grepl('\\<latitude\\>', names(file_raw), ignore.case = T))) {
          } else{
            file_raw <-
              file_raw %>%
              dplyr::rename(!!'lat' := (names(file_raw)[grepl('\\<latitude\\>', names(file_raw), ignore.case = T)])[1])
          }
          if (!any(grepl('\\<longitude\\>', names(file_raw), ignore.case = T))) {
          } else{
            file_raw <-
              file_raw %>%
              dplyr::rename(!!'lon' := (names(file_raw)[grepl('\\<longitude\\>', names(file_raw), ignore.case = T)])[1])
          }

          available_years <- names(file_raw)[grepl(paste0(time_periods, collapse = '|'), names(file_raw))]
          file_raw <- file_raw[, c('lon', 'lat', paste0(available_years)), with = FALSE]

          # Replace with helios::mapping_wrf_us49
          pop_df <- file_raw %>%
            dplyr::mutate(across(c(lat, lon), ~ round(., 5))) %>%
            tidyr::pivot_longer(cols = -c('lat', 'lon'),
                                names_to = 'year',
                                values_to = 'value') %>%
            tibble::as_tibble() %>%
            dplyr::mutate(year = as.integer(year)) %>%
            dplyr::select(lat, lon, year, value)

        } # end of CSV file processing

        # ------------------------------------------------
        # read other population (Currently support 1/8th NetCDF from SEDAC)
        # ------------------------------------------------
        else if (any(file_type %in% c('nc', 'nc4'))) {

          # raster_base <- raster::raster(resolution = 0.5,
          #                               xmn = -180, xmx = 180, ymn = -56, ymx = 84,
          #                               crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

          pop_in <- ncdf4::nc_open(file)

          var_names <- names(pop_in$var)

          pop_df <- data.frame()

          for (var in var_names){
            year <- strsplit(var, '_')[[1]][2]

            pop_brick <- raster::brick(file, varname = var, ncdf=TRUE)

            n_layers <- raster::nlayers(pop_brick)

            for(i in n_layers){

              ras <- pop_brick[[i]]

              pop_temp <- raster::as.data.frame(ras, xy = TRUE) %>%
                dplyr::rename(lat = y,
                              lon = x,
                              value = layer) %>%
                dplyr::mutate(value = as.numeric(tidyr::replace_na(value, 0)),
                              year = as.integer(year)) %>%
                dplyr::filter(year %in% time_periods) %>%
                tibble::as_tibble()

              pop_df <- dplyr::bind_rows(pop_df, pop_temp)

            }

            # raster_base <- raster::raster(resolution = 0.5,
            #                               xmn = ceiling(pop_brick@extent@xmin),
            #                               xmx = ceiling(pop_brick@extent@xmax),
            #                               ymn = ceiling(pop_brick@extent@ymin),
            #                               ymx = ceiling(pop_brick@extent@ymax),
            #                               crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
            #
            # resolution <- raster::res(pop_brick)[1]
            #
            # pop_agg <- raster::aggregate(pop_brick, fact = 0.5 / resolution, fun = sum)
            #
            # pop_agg_resample <- raster::resample(pop_agg, raster_base, method = 'bilinear')

          }

          ncdf4::nc_close(pop_in)

        } # end of NetCDF file processing

        else {
          stop('The file type is invalid. Please provide a valid file.')
        }

        return(pop_df)

      } else {
        stop(paste0('Population file provided: ', file, ' does not exist.'))
        file_raw = 'No population'
      }
    }
  } else if (any(grepl('tbl_df|tbl|data.frame', class(file)))) {
    file_raw = file
  }

} # end of read_population function
