#' pkg_example
#'
#' list example file paths
#'
#' @param path Default = NULL. Path to example files
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

pkg_example <- function(path = NULL) {

  if (is.null(path)) {
    dir(system.file('extras', package = 'helios'))
  } else {
    system.file('extras', path, package = 'helios', mustWork = TRUE)
  }

}


#' create_name
#'
#' create file names
#'
#' @param str_vec Default = NULL. vector of strings within the name
#' @param file_ext Default = NULL. extension name of the file. If NULL, will be a folder name
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

create_name <- function(str_vec = NULL, file_ext = NULL) {
  new_vec <- str_vec[nzchar(str_vec)]

  if(is.null(file_ext)){
    name <- paste(new_vec, collapse = '_')
  } else {
    name <- paste0(paste(new_vec, collapse = '_'), '.', file_ext)
  }

  return(name)
}


#' find_mapping_grid
#'
#' map shape attributes (region names) to grid
#'
#'@param data Default = NULL. data frame with lat and lon
#' @param spatial Default = NULL. Options: 'states_us_49', 'gcam_regions_32'. Aggregate to different spatial boundaries.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

find_mapping_grid <- function(data = NULL, spatial = NULL) {

  NULL -> geometry -> intersection -> X -> Y -> region -> subRegion -> ID

  if(is.null(spatial)) {
    stop('Must provide spatial scales.')
  }

  if(any(is.null(data), !c('lat', 'lon') %in% names(data))) {
    stop('Must provide valide table with both lat and lon columns')
  }

  grid <- data %>%
    dplyr::select(lat, lon) %>%
    dplyr::distinct()

  lat <- grid$lat
  lon <- grid$lon

  # function to find region, subregion, and ID from map based on lat and lon
  mapping_grid <- function(grid, shape){

    pnts_sf <- sf::st_as_sf(grid,
                            coords = c('lon', 'lat'),
                            crs = sf::st_crs(shape))


    pnts_sf <- pnts_sf %>%
      dplyr::mutate(
        intersection = as.character(sf::st_intersects(geometry, shape)),
        intersection = as.numeric(sub("\\D*(\\d+).*", "\\1", intersection))
    )

    pnts_sf$intersection[pnts_sf$intersection == 0] <- NA

    pnts_sf <- pnts_sf %>%
      dplyr::mutate(ID = dplyr::if_else(is.na(intersection), '', shape$subRegionAlt[intersection]),
                    region = dplyr::if_else(is.na(intersection), '', shape$region[intersection]),
                    subRegion = dplyr::if_else(is.na(intersection), '', shape$subRegion[intersection]))

    pnts_df <- sf::st_coordinates(pnts_sf) %>%
      as.data.frame() %>%
      dplyr::rename(lon = X,
                    lat = Y)
    pnts_df$ID <- pnts_sf$ID
    pnts_df$region <- pnts_sf$region
    pnts_df$subRegion <- pnts_sf$subRegion

    return(pnts_df)

  }

  if(is.character(spatial)) {

    if(spatial == 'gcam_us49') {

      mapping <- helios::mapping_wrf_us49

      intersect <- grid %>%
        dplyr::inner_join(mapping %>% dplyr::select(lat, lon),
                          by = c('lat', 'lon'))

      if(nrow(intersect) == 0){
        mapping <- mapping_grid(grid = grid,
                                shape = rmap::mapUS49)
      }


    } else if (spatial == 'gcam_regions32') {

      mapping <- helios::mapping_grid_region

      intersect <- grid %>%
        dplyr::inner_join(mapping %>% dplyr::select(lat, lon))

      if(nrow(intersect) == 0){
        mapping <- mapping_grid(grid = grid,
                                shape = rmap::mapGCAMReg32)
      }

    } else if (spatial == 'gcam_regions31_us52'){

      mapping <- helios::mapping_grid_region_US52

      intersect <- grid %>%
        dplyr::inner_join(mapping %>% dplyr::select(lat, lon))

      if(nrow(intersect) == 0){
        mapping <- mapping_grid(grid = grid,
                                shape = rmap::mapGCAMReg32US52)
      }
    }
  } else if(any(class(spatial) %in% c("tbl_df","tbl","data.frame"))) {

    if ('subRegion' %in% names(spatial)){

      shape <- rmap::map_find(dataTbl = spatial)[[2]]

      mapping <- mapping_grid(grid = grid,
                              shape = shape)

    } else {
      stop('Must provide a subRegion column')
    } # end of if ('subRegion' %in% names(spatial))

  } else {
    stop('The spatial scale is not available')
  } # end of if(is.character(spatial))

  mapping[mapping == ''] <- NA
  mapping <- mapping %>%
    dplyr::filter(!is.na(region) | !is.na(subRegion))

  if(nrow(mapping) == 0) {
    stop(paste0('Climate data is not within the selected spatial map: ', spatial))
  }

  mapping_data <- data %>%
  dplyr::left_join(mapping, by = c('lat', 'lon')) %>%
    dplyr::filter(!is.na(ID))

  return(mapping_data)

}


#' is_regular
#'
#' check if the grids have regular or irregular spacing
#'
#' @param grid Default = NULL. data frame with lon and lat columns
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

is_regular <- function(grid = NULL){

  NULL -> check -> ID -> lon -> lon_lag -> lon_space -> lat -> lat_lag -> lat_space

  lon_resolution <- grid %>%
    dplyr::select(lon) %>%
    dplyr::distinct() %>%
    dplyr::arrange(lon) %>%
    dplyr::mutate(lon_lag=dplyr::lag(lon),
                  lon_space = lon - lon_lag) %>%
    dplyr::filter(!is.na(lon_space))
  lon_resolution <- unique(lon_resolution$lon_space)

  lat_resolution <- grid %>%
    dplyr::select(lat) %>%
    dplyr::distinct() %>%
    dplyr::arrange(lat) %>%
    dplyr::mutate(lat_lag=dplyr::lag(lat),
                  lat_space = lat - lat_lag) %>%
    dplyr::filter(!is.na(lat_space))
  lat_resolution <- unique(lat_resolution$lat_space)

  if(any(length(lon_resolution) > 1, length(lat_resolution) > 1)){
    check <- FALSE
  } else {
    check <- TRUE
  }

  return(check)
}


#' match_grids
#'
#' match population data to the same resolution and grids with climate data
#'
#' @param from_df Default = NULL. data frame with lat and lon columns to provide the original grids
#' @param to_df Default = NULL. data frame with lat and lon columns to provide the base grids to convert to
#' @param time_periods Defualt = NULL. integer vector. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

match_grids <- function(from_df = NULL, to_df = NULL, time_periods = NULL){

  NULL -> lon -> lat -> year -> value -> y -> x

  to_grid <- to_df %>%
    dplyr::select(lon, lat) %>%
    dplyr::distinct()

  from_grid <- from_df %>%
  dplyr::select(lon, lat) %>%
    dplyr::distinct()

  out <- tryCatch(terra::rast(to_grid), error = function(e) e)
  is_to_regular <- !any(class(out) == 'error')

  out <- tryCatch(terra::rast(from_grid), error = function(e) e)
  is_from_regular <- !any(class(out) == 'error')

  if(all(is_to_regular, is_from_regular)) {
    to_ras <- terra::rast(to_grid)
    to_res <- unique(terra::res(to_ras))

    from_ras <- terra::rast(from_grid)
    from_res <- unique(terra::res(from_ras))

    if(to_res != from_res){

      out <- data.frame()

      for(yr in time_periods){


        from_grid <- from_df %>%
          dplyr::filter(year == yr) %>%
          dplyr::select(lon, lat, value) %>%
          dplyr::distinct()

        from_ras <- terra::rast(from_grid)

        if(to_res > from_res){

          agg <- terra::aggregate(from_ras, fact = to_res / from_res, fun = sum)

          agg_resample <- terra::resample(agg, to_ras, method = 'bilinear')

        } else {

          agg <- terra::disagg(from_ras, fact = from_res / to_res, fun = sum) / (from_res / to_res)^2

          agg_resample <- terra::resample(agg, to_ras, method = 'bilinear')

        }

        temp <- terra::as.data.frame(agg_resample, xy = TRUE, na.rm = FALSE) %>%
          dplyr::rename(lat = y,
                        lon = x) %>%
          dplyr::mutate(year = yr) %>%
          tibble::as_tibble()

        out <- dplyr::bind_rows(out, temp)

      }# end of for(year in time_periods)

    } else {

      print(paste0('The resolution for both climate and opulation data is the same: ', to_res))
      out <- from_df

    }

  } else {

    message('Cannot map irregular grid cells. Skip matching population to climate grid cells.')

    out <- from_df

  } # end of if(all(is.regular(to_grid), is.regular(from_grid)))


  return(out)
}
