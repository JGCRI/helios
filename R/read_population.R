#' read_population
#'
#' Process population file depends on the input
#'
#' @param file Default = NULL. Path to ncdf file
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export


read_population <- function(file = NULL) {

  NULL -> ID -> region -> subRegion -> lat -> lon -> across -> RID -> x -> y ->
    layer -> value

  if (!is.null(file)){

    # If isn't a dataframe check if file exists
    if (length(file) == 1) {
      if (any(class(file) == "character")) {
        if (file.exists(file)) {
          file_type <- strsplit(basename(file), split="\\.")[[1]][2]

          # ------------------------------------------------
          # directly read corresponding wrd population
          # ------------------------------------------------
          if (any(file_type %in% 'csv')) {

            file_raw <- data.table::fread(file)

            # Rename latitude and longitude if needed
            if (!any(grepl("\\<latitude\\>", names(file_raw), ignore.case = T))) {
            } else{
              file_raw <-
                file_raw %>%
                dplyr::rename(!!"lat" := (names(file_raw)[grepl("\\<latitude\\>", names(file_raw), ignore.case = T)])[1])
            }
            if (!any(grepl("\\<longitude\\>", names(file_raw), ignore.case = T))) {
            } else{
              file_raw <-
                file_raw %>%
                dplyr::rename(!!"lon" := (names(file_raw)[grepl("\\<longitude\\>", names(file_raw), ignore.case = T)])[1])
            }

            # Replace with helios::mapping_wrf_us49
            pop_df <- helios::mapping_wrf_us49 %>%
              dplyr::select(ID, region, subRegion, lat, lon) %>%
              dplyr::left_join(file_raw %>%
                                 dplyr::mutate(across(c(lat, lon), ~ round(., 5))),
                               by = c("lat", "lon")) %>%
              tidyr::gather(key = "year",
                            value = "value",
                            -RID, -lat, -lon, -region, -ID, -subRegion) %>%
              tibble::as_tibble() %>%
              dplyr::select(-RID)

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

              raster_base <- raster::raster(resolution = 0.5,
                                            xmn = ceiling(pop_brick@extent@xmin),
                                            xmx = ceiling(pop_brick@extent@xmax),
                                            ymn = ceiling(pop_brick@extent@ymin),
                                            ymx = ceiling(pop_brick@extent@ymax),
                                            crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

              resolution <- raster::res(pop_brick)[1]

              pop_agg <- raster::aggregate(pop_brick, fact = 0.5 / resolution, fun = sum)

              pop_agg_resample <- raster::resample(pop_agg, raster_base, method = 'bilinear')

              pop_temp <- raster::as.data.frame(pop_agg_resample, xy = TRUE, na.rm = FALSE) %>%
                dplyr::rename(lat = y,
                              lon = x,
                              value = layer) %>%
                tibble::as_tibble() %>%
                dplyr::left_join(helios::mapping_grid_region, by = c('lat', 'lon')) %>%
                dplyr::filter(!is.na(ID)) %>%
                dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value),
                              year = year)

              pop_df <- dplyr::bind_rows(pop_df, pop_temp)
            }

            ncdf4::nc_close(pop_in)

          } # end of NetCDF file processing

          else {
            stop('The file type is invalid. Please provide a valid file.')
          }

          return(pop_df)

        } else {
          stop(paste0("Population file provided: ", file, " does not exist."))
          file_raw = "No population"
        }
      }
    } else if (any(grepl("tbl_df|tbl|data.frame", class(file)))) {
      file_raw = file
    }


  } else {
    stop('Please provide file path.')
  }

} # end of read_population function
