#' read_ncdf
#'
#' Process standard NetCDF files from ISIMIP or WRF
#' Read Temperature
#'
#' @param ncdf Default = NULL. Path to ncdf file
#' @param model Default = NULL. Options are 'cmip' (ISIMIP based), 'wrf'
#' @param var Default = NULL. Climate variable to extract. Temperature var is 'tas' for ISIMIP; 'T2' for WRF
#' @param time_periods Default = NULL. Vector of years selected to process.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export


read_ncdf <- function(ncdf = NULL,
                      model = NULL,
                      var = NULL,
                      time_periods = NULL) {

  NULL -> ID -> X1 -> x -> y -> across -> setNames

  if (!is.null(model)) {

    # Process ISIMIP NetCDF
    if (model == 'cmip') {

      ncdf_in <- ncdf4::nc_open(ncdf)
      var_names <- attributes(ncdf_in$var)$names

      dims <- c(ncdf_in$var[[var]]$dim[[1]]$name,
                ncdf_in$var[[var]]$dim[[2]]$name,
                ncdf_in$var[[var]]$dim[[3]]$name)
      dims_order <- match(c('lon', 'lat', 'time'), dims)

      if(!any(var %in% var_names)){
        stop('Climate variable name is not valid. Please provide valid ncdf_var name.')
      }

      # get the data time series
      ncdf_brick <- raster::brick(ncdf, varname = var, ncdf = TRUE,
                                  dims = dims_order, stopIfNotEqualSpaced = F)

      # Base raster
      ncdf_ras <- ncdf_brick[[1]]

      # Get layer names
      name_brick <- names(ncdf_brick)

      # get date time
      ncdf_times <- gsub('\\.', '-', gsub('X', '', name_brick))

      # get time series index
      time_index <- seq(1, length(ncdf_times), 1)

      # # get time series
      # time_index <- ncdf4::ncvar_get(ncdf_in, 'time')
      #
      # time_start <- time_end <- as.POSIXlt(
      #   ncdf_ras@z[[1]],
      #   tz = 'UTC')
      # time_end$mday <- time_start$mday + time_index[length(time_index)]
      # ncdf_times <- format(seq(time_start, time_end, by = '1 day'), '%Y-%m-%d')

      # subset ncdf times within selected periods
      indices <- as.integer(grepl(paste0(time_periods, collapse = '|'), ncdf_times))
      index_subset <- c(1:length(ncdf_times)) * indices
      index_subset <- index_subset[!index_subset %in% 0]
      ncdf_times <- ncdf_times[index_subset]

      ncdf_brick <- ncdf_brick[[index_subset]]


      #......................
      # Step 1: Map grid (lat/lon) to each shape in the polygons being mapped to
      #......................

      # Lat and Lon from ncdf
      lat <- ncdf4::ncvar_get(ncdf_in, 'lat')
      lon <- ncdf4::ncvar_get(ncdf_in, 'lon')
      latlon <- data.frame(lat = rep(lat, each = length(lon)),
                           lon = rep(lon, times = length(lat)))
      latlon_intersect <- helios::mapping_grid_region %>%
        dplyr::select(lat, lon) %>%
        dplyr::intersect(latlon)

      ncdf_dim <- sp::SpatialPoints(cbind(lon = latlon_intersect$lon,
                                          lat = latlon_intersect$lat),
                                    proj4string = raster::crs(ncdf_ras))

      ncdf_brick_df <- dplyr::bind_cols(
        raster::as.data.frame(raster::extract(x = ncdf_brick, y = ncdf_dim, sp = T))) %>%
        tibble::as_tibble()

      ncdf_grid <- ncdf_brick_df %>%
        dplyr::rename(setNames(c(name_brick, 'lat', 'lon'),
                               c(ncdf_times, 'lat', 'lon')))

    } # end of isimip netcdf processing

    # Process WRF NetCDF
    else if (model == 'wrf') {

      ncdf_in <- ncdf4::nc_open(ncdf)
      var_names <- attributes(ncdf_in$var)$names

      dims <- c(ncdf_in$var[[var]]$dim[[1]]$name,
                ncdf_in$var[[var]]$dim[[2]]$name,
                ncdf_in$var[[var]]$dim[[3]]$name)
      dims_order <- match(c('west_east', 'south_north', 'Time'), dims)

      if(!any(var %in% var_names)){
        stop('Climate variable name is not valid. Please provide valid ncdf_var name.')
      }

      ncdf_brick <- raster::brick(ncdf, varname = var, ncdf = TRUE,
                                  dims = dims_order, stopIfNotEqualSpaced = F)

      # Index of Times available
      ncdf_times <-  ncdf4::ncvar_get(ncdf_in, 'Times')

      ncdf4::nc_close(ncdf_in)

      # subset ncdf times within selected periods
      indices <- as.integer(grepl(paste0(time_periods, collapse = '|'), ncdf_times))
      index_subset <- c(1:length(ncdf_times)) * indices
      index_subset <- index_subset[!index_subset %in% 0]
      ncdf_times <- ncdf_times[index_subset]

      ncdf_brick <- ncdf_brick[[index_subset]]

      #......................
      # Step 1: Map grid (lat/lon) to each shape in the polygons being mapped to
      #......................

      # Base raster
      ncdf_ras <- ncdf_brick[[1]]

      # Lat and Lon from ncdf
      ncdf_lat <- (raster::brick(ncdf, varname = 'XLAT', ncdf = TRUE))[[1]]
      ncdf_lon <- (raster::brick(ncdf, varname = 'XLONG', ncdf = TRUE))[[1]]

      # Get Lat long
      ncdf_lat_df <-
        raster::as.data.frame(ncdf_lat, xy = TRUE, na.rm = TRUE) %>%
        dplyr::rename(lat = X1)
      ncdf_lon_df <-
        raster::as.data.frame(ncdf_lon, xy = TRUE, na.rm = TRUE) %>%
        dplyr::rename(lon = X1)

      # Convert to a table with original ids (x,y)
      ncdf_dim <- ncdf_lat_df %>%
        dplyr::select(x, y)

      # Get layer names
      name_brick <- names(ncdf_brick)

      ncdf_brick_df <- dplyr::bind_cols(
        raster::as.data.frame(raster::extract(x = ncdf_brick, y = ncdf_dim, sp = T)),
        ncdf_dim) %>%
        tibble::as_tibble() %>%
        dplyr::left_join(ncdf_lat_df, by = c('x', 'y')) %>%
        dplyr::left_join(ncdf_lon_df, by = c('x', 'y')) %>%
        dplyr::select(-x, -y) %>%
        dplyr::mutate(across(c(lat, lon), ~ round(., 5)))

      ncdf_grid <- ncdf_brick_df %>%
        dplyr::rename(setNames(c(name_brick, 'lat', 'lon'),
                               c(ncdf_times, 'lat', 'lon')))

    }# end of wrf netcdf processing

    else{
      stop('The model name is invalid. Please provide a valid model name. Options: wrf, cmip')
    }

    return(ncdf_grid)

  } else {
    stop('Please provide the model name where the data is from.')
  }


} # end of function
