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
#' @param spatial Default = NULL. Options: 'states_us_49', 'gcam_regions_32'. Aggregate to different spatial boundaries.
#' @param lat Default = NULL. Latitudes of the grid cells
#' @param lon Default = NULL. Longitudes of the grid cells
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

find_mapping_grid <- function(spatial = NULL, lat = NULL, lon = NULL) {

  NULL -> geometry -> intersection -> X -> Y -> region -> subRegion

  if(is.null(spatial)) {
    stop('Must provide spatial scales.')
  }

  if(any(is.null(lat), is.null(lon))) {
    stop('Must provide both latitudes and longitudes.')
  }

  grid <- data.frame(lat = lat,
                     lon = lon)

  # function to find region, subregion, and ID from map based on lat and lon
  mapping_grid <- function(grid, shape){

    pnts_sf <- sf::st_as_sf(grid,
                            coords = c('lon', 'lat'),
                            crs = sf::st_crs(shape))

    pnts_sf <- pnts_sf %>%
      dplyr::mutate(
        intersection = as.integer(sf::st_intersects(geometry, shape)),
        ID = dplyr::if_else(is.na(intersection), '', shape$subRegionAlt[intersection]),
        region = dplyr::if_else(is.na(intersection), '', shape$region[intersection]),
        subRegion = dplyr::if_else(is.na(intersection), '', shape$subRegion[intersection])
    )

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

      if(!all(cbind(lat = lat, lon = lon) %in%
              cbind(lat = mapping$lat, lon = mapping$lon))){
        mapping <- mapping_grid(grid = grid,
                                shape = rmap::mapUS49)
      }


    } else if (spatial == 'gcam_regions32') {

      mapping <- helios::mapping_grid_region

      if(!all(cbind(lat = lat, lon = lon) %in%
              cbind(lat = mapping$lat, lon = mapping$lon))){
        mapping <- mapping_grid(grid = grid,
                                shape = rmap::mapGCAMReg32)
      }

    } else if (spatial == 'gcam_regions31_us52'){

      mapping <- helios::mapping_grid_region_US52

      if(!all(cbind(lat = lat, lon = lon) %in%
              cbind(lat = mapping$lat, lon = mapping$lon))){
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

  return(mapping)

}
