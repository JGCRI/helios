#' hdcd
#'
#' Heating and Cooling Degree processing for GCAM from various sources such as WRF and CMIP
#'
#' @param ncdf Default = NULL. String or vector for paths to the NetCDF file.
#' @param ncdf_var Default = NULL. String for variable name to extract from NetCDF file. Temperature var is 'tas' for CMIP models; 'T2' for WRF model.
#' @param model Default = NULL. String for climate model that generates the ncdf file. Options: 'wrf' or 'cmip'.
#' @param model_timestep Default = NULL. String for time step of input climate data. Options: 'hourly' or 'daily'
#' @param population Default = NULL. String for path to population files (NetCDF or CSV). The CSV file need to have columns latitude, longitude, and years. For example,  [latitude, longitude, 2020, 2021, ...]
#' @param spatial Default = NULL. String for spatial aggregation boundaries. Options: check helios::spatial_options. 'gcam_us49', 'gcam_regions32', 'gcam_regions31_us52', 'gcam_countries', 'gcam_basins'.
#' @param time_periods Default = NULL. Integer vector for selected time periods to process. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @param dispatch_segment Default = FALSE. Set to TRUE to output degree-hours by GCAM-USA dispatch segment. This can only be TRUE when model time_step is set to 'hourly'.
#' @param reference_temp_F Default = 65. Integer for comfort temperature in degree F. 65 degree F is the comfort baseline temperature typically used by NOAA. The comfort temperature can vary by regions.
#' @param folder Default = paste0(getwd(),'/output'). String for output folder path.
#' @param diagnostics Default = FALSE. Set to TRUE to create diagnostic figures.
#' @param xml Default = FALSE. Set to TRUE to generate XML outputs for GCAM.
#' @param save Default = TRUE. Set to TRUE to save outputs.
#' @param name_append Default = ''. String for the name to append to output file name.
#' @param im3_analysis Default = T. Output annual HDCD at grid region scale for trend-representative year analysis
#' @param elec_share Default = NULL. data frame for the fraction of building heating and cooling energy consumption met by electricity at grid region scale. Column [subRegion, year, HDCD, elec_frac]. If elec_share is provided, helios will recalculate the super peak hours; Otherwise, helios will use the default super peak hours. Note, to get the correct super peak hours, the ncdf argument should include all files that cover full years.
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

hdcd <- function(ncdf = NULL,
                 ncdf_var = NULL,
                 model = NULL,
                 model_timestep = NULL,
                 population = NULL,
                 spatial = NULL,
                 time_periods = NULL,
                 dispatch_segment = FALSE,
                 reference_temp_F = 65,
                 folder = file.path(getwd(), 'output'),
                 diagnostics = F,
                 xml = F,
                 name_append = '',
                 save = T,
                 im3_analysis = T,
                 elec_share = NULL) {

  print('Starting function process_hdcd...')

  #......................
  # Initialize
  #......................

  if(T){

    NULL -> RID -> subRegion_total_value -> pop_weight -> stateCode ->
      HDCD -> scenario -> ID -> V3 -> day -> unit ->
      building.node.input -> gcam.consumer -> heatcool -> month -> nodeInput ->
      segment -> subRegion -> thermal.building.service.input -> value ->
      x -> y -> year -> datetime -> region -> lat -> lon -> hour -> HD -> CD

    if(is.null(folder)) {
      folder <- paste0(getwd(), '/output')
    }

    if (save | diagnostics | xml) {
      if (!dir.exists(folder)) {
        dir.create(folder)
      }
    }

    # Pick up on intermediate files if program crashed before.
    hdcd_comb_gcam <- tibble::tibble()
    hdcd_comb_monthly <- tibble::tibble()
    hdcd_comb_annual <- tibble::tibble()
    hdcd_region <- tibble::tibble()
    hdcd_region_bld <- tibble::tibble()
    hdcd_region_monthly <- tibble::tibble()
    hdcd_region_annual <- tibble::tibble()

    # for IM3 grid region analysis
    if(im3_analysis){
      hdcd_gridregion <- tibble::tibble()
      hdcd_comb_gridregion_gcam <- tibble::tibble()
      hdcd_comb_gridregion_monthly <- tibble::tibble()
      hdcd_comb_gridregion_annual <- tibble::tibble()
    }



    # check population input format
    if (any(grepl('tbl_df|tbl|data.frame', class(population)))) {
      population <- list('pop' = population)
    }

    # check if there is valid model_step input
    if(any(is.null(model_timestep), !model_timestep %in% c('hourly', 'daily'))){
      stop('Please provide model time step. Options: daily or hourly')
    }

    # check if there is valid spatial input
    if(any(is.null(spatial),
           !any(spatial %in% helios::spatial_options$spatial, class(spatial) %in% c("tbl_df","tbl","data.frame")))){
      stop(paste0('Please provide a valid spatial scale. Options: ',
                  paste0(helios::spatial_options$spatial, collapse = ', ')))
    }


  }

  #......................
  # Loop over each ncdf file
  #......................

  print(paste0('Processing files provided: ', paste0(ncdf, collapse = ', ')))

  for(i in 1:length(ncdf)){

    ncdf_i <- ncdf[i]

    if(!file.exists(ncdf_i)){

      stop(paste0('File does not exist: ', ncdf_i))

    } # end of if(file.exists(ncdf_i))

    for(j in 1:length(population)){

      #......................
      # Step 1: Process Temperature netCDF file
      #......................
      # Assign time_periods
      if (is.null(time_periods)) {
        message('Setting time periods to default 2020 to 2100 with 5 year interval.')
        time_periods <- seq(2020, 2100, by = 5)

      } else {

        if (all(time_periods == floor(time_periods))) {

          time_periods <- time_periods

        } else {
          stop('Please provide valid vector. For example, c(2020, 2030).')
        }

      }

      print('.........................................')
      print(paste0('Running hdcd for file: ', ncdf_i))

      ncdf_grid <- helios::process_temperature(ncdf = ncdf_i,
                                               model = model,
                                               ncdf_var = ncdf_var,
                                               time_periods = time_periods,
                                               spatial = spatial,
                                               reference_temp_F = reference_temp_F)


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


      #......................
      # Step 2: Population weighted grid
      #......................

      if (!is.null(population)) {

        population_j = population[[j]]

        # process population and match to ncdf lat and lons
        pop_list <- helios::process_population(population = population_j,
                                               coordinates = ncdf_pivot,
                                               time_periods = time_periods,
                                               climate_years = years,
                                               spatial = spatial,
                                               im3_analysis = im3_analysis)

        population_j_weighted <- pop_list$population_weighted
        population_j_weighted_gridregion <- pop_list$population_weighted_gridregion


        #......................
        # Step 3: Calculate Heating and Cooling Degrees (Kelvin to F)
        # Step 4: Population weight for each grid for each year
        #......................


        ncdf_hdcd_pop_weighted <- ncdf_pivot %>%
          dplyr::left_join(population_j_weighted %>%
                             dplyr::select(-value, -subRegion_total_value),
                           by = c('ID', 'region', 'subRegion', 'lat', 'lon', 'year')) %>%
          dplyr::mutate(value = dplyr::if_else(is.na(pop_weight), value, value * pop_weight))

        # aggregated to region, but leave the datetime for later segment, monthly, annual aggregation
        hdcd_region_i <- ncdf_hdcd_pop_weighted %>%
          dplyr::group_by(region, subRegion, ID, year, datetime) %>%
          dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
          dplyr::ungroup()

        # combine results from all the input ncdf_i
        hdcd_region <- dplyr::bind_rows(hdcd_region, hdcd_region_i)

        # for IM3 grid region analysis
        if(im3_analysis){

          ncdf_pivot_gridregion <- ncdf_pivot %>%
            dplyr::left_join(helios::mapping_states_gridregion, by = 'subRegion') %>%
            dplyr::mutate(subRegion = grid_region,
                          ID = grid_region) %>%
            dplyr::select(-grid_region)

          ncdf_hdcd_pop_weighted_gridregion <- ncdf_pivot_gridregion %>%
            dplyr::left_join(population_j_weighted_gridregion %>%
                               dplyr::select(-value, -subRegion_total_value),
                             by = c('ID', 'region', 'subRegion', 'lat', 'lon', 'year')) %>%
            dplyr::mutate(value = (((value - 273.15) * 9/5) + 32) - reference_temp_F,
                          value = dplyr::if_else(is.na(pop_weight), value, value * pop_weight))

          # aggregated to region, but leave the datetime for later segment, monthly, annual aggregation
          hdcd_gridregion_i <- ncdf_hdcd_pop_weighted_gridregion %>%
            dplyr::group_by(region, subRegion, ID, year, datetime) %>%
            dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
            dplyr::ungroup()

          hdcd_gridregion <- dplyr::bind_rows(hdcd_gridregion, hdcd_gridregion_i)
        }


      } else {
        stop('Please provide valide population file path.')
      }

      print(paste0('Processing hdcd completed for file: ', ncdf_i))

    } # end of for(j in 1:length(population))

  } # Close for(i in 1:length(ncdf)){

  #......................
  # Step 5: Subset for time periods chosen
  #......................

  # Subset raster brick to selected times
  if (length(index_subset) > 0) {

    if (model_timestep == 'hourly') {

      # Equivalent to step 6: Aggregate to regions
      # hdcd_region <- ncdf_hdcd_pop_weighted %>%
      #   dplyr::filter(!is.na(subRegion)) %>%
      #   dplyr::select(-lat, -lon) %>%
      #   dplyr::group_by(region, subRegion, ID, year, datetime) %>%
      #   dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
      #   dplyr::ungroup()

      # Assign HDCD categories
      hdcd_region <- hdcd_region %>%
        dplyr::mutate(HDCD = dplyr::if_else(value < 0, 'HD', 'CD')) %>%
        dplyr::filter(value != 0)

      #......................
      # Step 6a: Aggregate over Segments, monthly and annual
      #......................

      ncdf_times_all <- unique(hdcd_region$datetime)

      temporal_subset <- data.frame(ncdf_times = ncdf_times_all) %>%
        dplyr::mutate(datetime = as.POSIXct(ncdf_times, format = '%Y-%m-%d_%H:%M:%S', tz = 'UTC')) %>%
        dplyr::mutate(year = lubridate::year(datetime),
                      month = lubridate::month(datetime),
                      day = lubridate::day(datetime),
                      hour = lubridate::hour(datetime),
                      timezone = lubridate::tz(datetime)) %>%
        dplyr::mutate(month = dplyr::if_else(month < 10, paste0('0', month), paste0(month)),
                      day = dplyr::if_else(day < 10, paste0('0', day), paste0(day)),
                      hour = dplyr::if_else(hour < 10, paste0('0', hour), paste0(hour)) )

      # hdcd segments degree hours
      if(!is.null(elec_share) & im3_analysis){

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

        # if elec_share is provided, calculate the super peak hours that have
        # the 10 highest HDH (or CDH)
        elec_share_region <- elec_share %>%
          dplyr::select(subRegion, year, HDCD, elec_frac)

        # if the elec_share does not cover all the time_periods
        # need to expand the elec_share to cover all the time_periods
        elec_share_region <- elec_share_region %>%
          tidyr::expand(tidyr::nesting(subRegion, HDCD),
                        year = time_periods) %>%
          dplyr::left_join(mapping_time_period, by = 'year') %>%
          dplyr::left_join(elec_share_region, by = c('subRegion', 'HDCD', 'period' = 'year')) %>%
          dplyr::select(grid_region = subRegion, year, HDCD, elec_frac)

        # for state level calculation
        elec_share_region <- helios::mapping_states_gridregion %>%
          dplyr::left_join(elec_share_region, by = c('grid_region')) %>%
          dplyr::select(subRegion, year, HDCD, elec_frac)

        # calculate the HDCD
        hdcd_region_segments <- hdcd_region %>%
          dplyr::left_join(temporal_subset, by = c('datetime', 'year')) %>%
          dplyr::left_join(helios::segment_map_utc_no_superpeak,
                           by = c('subRegion', 'month', 'day', 'hour')) %>%
          dplyr::left_join(elec_share_region, by = c('subRegion', 'year', 'HDCD')) %>%
          dplyr::mutate(value_elec = abs(value * elec_frac)) %>%
          dplyr::group_by(region, subRegion, year) %>%
          dplyr::mutate(value_rank = order(order(value_elec, decreasing = TRUE)),
                        segment = ifelse(value_rank <= 10, 'superpeak', segment)) %>%
          dplyr::ungroup()

        # create new segment map
        segment_map_utc_region <- hdcd_region_segments %>%
          dplyr::select(subRegion, year, segment, month, day, hour) %>%
          dplyr::distinct()

        # aggregate by segment and HDCD
        hdcd_region_segments <- hdcd_region_segments %>%
          dplyr::group_by(region, subRegion, year, segment, HDCD) %>%
          dplyr::summarise(value = sum(value, na.rm = T)) %>%
          dplyr::ungroup() %>%
          dplyr::select(region, subRegion, year, segment, HDCD, value)


      } else {

        # if elec_share is not provided, use the default segment mapping with default super peak
        hdcd_region_segments <- hdcd_region %>%
          dplyr::left_join(temporal_subset, by = c('datetime', 'year')) %>%
          dplyr::left_join(helios::segment_map_utc,
                           by = c('subRegion', 'month', 'day', 'hour')) %>%
          dplyr::group_by(region, subRegion, year, segment, HDCD) %>%
          dplyr::summarise(value = sum(value, na.rm = T)) %>%
          dplyr::ungroup()

      }


      # hdcd monthly degree days
      hdcd_region_monthly <- hdcd_region %>%
        dplyr::left_join(temporal_subset, by = c('datetime', 'year')) %>%
        dplyr::group_by(region, subRegion, year, month, day) %>%
        dplyr::summarise(value = (max(value) + min(value)) / 2) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(region, subRegion, year, month) %>%
        dplyr::summarise(HD = sum(value[value < 0]),
                         CD = sum(value[value > 0])) %>%
        dplyr::ungroup() %>%
        tidyr::gather(key = 'HDCD', value = 'value', HD, CD)

      # hdcd annual degree days
      hdcd_region_annual <- hdcd_region %>%
        dplyr::left_join(temporal_subset, by = c('datetime', 'year')) %>%
        dplyr::group_by(region, subRegion, year, month, day) %>%
        dplyr::summarise(value = (max(value) + min(value)) / 2) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(region, subRegion, year) %>%
        dplyr::summarise(HD = sum(value[value < 0]),
                         CD = sum(value[value > 0])) %>%
        dplyr::ungroup() %>%
        tidyr::gather(key = 'HDCD', value = 'value', HD, CD)


      # for IM3 grid region analysis
      if(im3_analysis){

        # Equivalent to step 6: Aggregate to regions
        # hdcd_gridregion <- ncdf_hdcd_pop_weighted_gridregion %>%
        #   dplyr::filter(!is.na(subRegion)) %>%
        #   dplyr::select(-lat, -lon) %>%
        #   dplyr::group_by(region, subRegion, ID, year, datetime) %>%
        #   dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
        #   dplyr::ungroup()

        # Assign HDCD categories
        hdcd_gridregion <- hdcd_gridregion %>%
          dplyr::mutate(HDCD = dplyr::if_else(value < 0, 'HD', 'CD')) %>%
          dplyr::filter(value != 0)

        #......................
        # Step 6a: Aggregate over Segments, monthly and annual
        #......................

        ncdf_times_all <- unique(hdcd_gridregion$datetime)

        temporal_subset_gridregion <- data.frame(ncdf_times = ncdf_times_all) %>%
          dplyr::mutate(datetime = as.POSIXct(ncdf_times, format = '%Y-%m-%d_%H:%M:%S', tz = 'UTC')) %>%
          dplyr::mutate(year = lubridate::year(datetime),
                        month = lubridate::month(datetime),
                        day = lubridate::day(datetime),
                        hour = lubridate::hour(datetime),
                        timezone = lubridate::tz(datetime)) %>%
          dplyr::mutate(month = dplyr::if_else(month < 10, paste0('0', month), paste0(month)),
                        day = dplyr::if_else(day < 10, paste0('0', day), paste0(day)),
                        hour = dplyr::if_else(hour < 10, paste0('0', hour), paste0(hour)) )

        # hdcd segments **degree hours**
        if(!is.null(elec_share) & im3_analysis){

          # if elec_share is provided, calculate the super peak hours that have
          # the 10 highest HDH (or CDH)
          elec_share_gridregion <- elec_share %>%
            dplyr::select(subRegion, year, HDCD, elec_frac)

          # if the elec_share does not cover all the time_periods
          # need to expande the elec_share to cover all the time_periods
          elec_share_gridregion <- elec_share_gridregion %>%
            tidyr::expand(tidyr::nesting(subRegion, HDCD),
                          year = time_periods) %>%
            dplyr::left_join(mapping_time_period, by = 'year') %>%
            dplyr::left_join(elec_share_gridregion, by = c('subRegion', 'HDCD', 'period' = 'year')) %>%
            dplyr::select(subRegion, year, HDCD, elec_frac)

          # segment map for grid regions
          segment_map_utc_no_superpeak_gridregion <- helios::segment_map_utc_no_superpeak %>%
            dplyr::left_join(helios::mapping_states_gridregion, by = 'subRegion') %>%
            dplyr::select(-subRegion) %>%
            dplyr::distinct() %>%
            dplyr::rename(subRegion = grid_region)

          # calculate the HDCD
          hdcd_gridregion_segments <- hdcd_gridregion %>%
            dplyr::left_join(temporal_subset_gridregion, by = c('datetime', 'year')) %>%
            dplyr::left_join(segment_map_utc_no_superpeak_gridregion,
                             by = c('subRegion', 'month', 'day', 'hour')) %>%
            dplyr::left_join(elec_share_gridregion, by = c('subRegion', 'year', 'HDCD')) %>%
            dplyr::mutate(value_elec = abs(value * elec_frac)) %>%
            dplyr::group_by(region, subRegion, year) %>%
            dplyr::mutate(value_rank = order(order(value_elec, decreasing = TRUE)),
                          segment = ifelse(value_rank <= 10, 'superpeak', segment)) %>%
            dplyr::ungroup()

          # create new segment map
          segment_map_utc_gridregion <- hdcd_gridregion_segments %>%
            dplyr::select(subRegion, year, segment, month, day, hour) %>%
            dplyr::distinct()

          # aggregate by segment and HDCD
          hdcd_gridregion_segments <- hdcd_gridregion_segments %>%
            dplyr::group_by(region, subRegion, year, segment, HDCD) %>%
            dplyr::summarise(value = sum(value, na.rm = T)) %>%
            dplyr::ungroup() %>%
            dplyr::select(region, subRegion, year, segment, HDCD, value)


        } else {

          # segment map for grid regions
          segment_map_utc_gridregion <- helios::segment_map_utc %>%
            dplyr::left_join(helios::mapping_states_gridregion, by = 'subRegion') %>%
            dplyr::select(-subRegion) %>%
            dplyr::distinct() %>%
            dplyr::rename(subRegion = grid_region)

          # hdcd segments **degree hours**
          hdcd_gridregion_segments <- hdcd_gridregion %>%
            dplyr::left_join(temporal_subset_gridregion, by = c('datetime', 'year')) %>%
            dplyr::left_join(segment_map_utc_gridregion,
                             by = c('subRegion', 'month', 'day', 'hour')) %>%
            dplyr::group_by(region, subRegion, year, segment, HDCD) %>%
            dplyr::summarise(value = sum(value, na.rm = T)) %>%
            dplyr::ungroup()

        }


        # hdcd monthly **degree hours**
        hdcd_gridregion_monthly <- hdcd_gridregion %>%
          dplyr::left_join(temporal_subset_gridregion, by = c('datetime', 'year')) %>%
          # dplyr::group_by(region, subRegion, year, month, day) %>%
          # dplyr::summarise(value = (max(value) + min(value)) / 2) %>%
          # dplyr::ungroup() %>%
          dplyr::group_by(region, subRegion, year, month) %>%
          dplyr::summarise(HD = sum(value[value < 0]),
                           CD = sum(value[value > 0])) %>%
          dplyr::ungroup() %>%
          tidyr::gather(key = 'HDCD', value = 'value', HD, CD)

        # hdcd annual **degree hours**
        hdcd_gridregion_annual <- hdcd_gridregion %>%
          dplyr::left_join(temporal_subset_gridregion, by = c('datetime', 'year')) %>%
          # dplyr::group_by(region, subRegion, year, month, day) %>%
          # dplyr::summarise(value = (max(value) + min(value)) / 2) %>%
          # dplyr::ungroup() %>%
          dplyr::group_by(region, subRegion, year) %>%
          dplyr::summarise(HD = sum(value[value < 0]),
                           CD = sum(value[value > 0])) %>%
          dplyr::ungroup() %>%
          tidyr::gather(key = 'HDCD', value = 'value', HD, CD)

      }


      #......................
      # Step 7a: Add in building components for GCAMUSA
      #......................

      if (dispatch_segment == TRUE){

        if(spatial == 'gcam_us49') {

          hdcd_region_bld <- hdcd_region_segments %>%
            dplyr::left_join(helios::L2441.HDDCDD_Fixed_gcamusa_seg,
                             by = c('subRegion', 'segment')) %>%
            # Remove columns with -ve hdcd/cooling and +ve/heating
            dplyr::filter(
              !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
              !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
            dplyr::select(-HDCD)


          # for IM3 analysis
          if(im3_analysis){

            # create building service structure for grid region
            L2441.HDDCDD_Fixed_gcamusa_seg_gridregion <- helios::L2441.HDDCDD_Fixed_gcamusa_seg %>%
              dplyr::left_join(helios::mapping_states_gridregion, by = 'subRegion') %>%
              dplyr::select(-subRegion) %>%
              dplyr::rename(subRegion = grid_region) %>%
              dplyr::distinct()

            # merge with segment table
            hdcd_gridregion_bld <- hdcd_gridregion_segments %>%
              dplyr::left_join(L2441.HDDCDD_Fixed_gcamusa_seg_gridregion,
                               by = c('subRegion', 'segment')) %>%
              # Remove columns with -ve hdcd/cooling and +ve/heating
              dplyr::filter(
                !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
                !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
              dplyr::select(-HDCD)

          }

        } else {
          # only gcam_us49 can have dispatch segment because
          # we don't have day, night segement for other regions
          stop(paste0('Dispatch segments cannot be applied to: ', spatial))

        }

      } else {

        # basic structure of building thermal service
        hdcd_building <- data.frame(
          gcam.consumer = c('comm', 'comm', 'resid', 'resid'),
          nodeInput = c('comm', 'comm', 'resid', 'resid'),
          building.node.input = c('comm_building', 'comm_building', 'resid_building', 'resid_building'),
          thermal.building.service.input = c('comm cooling', 'comm heating', 'resid cooling', 'resid heating'))

        region <- unique(hdcd_region_annual$region)

        # expand building info to each region
        hdcd_building <- hdcd_building %>%
          tidyr::expand(
            tidyr::nesting(gcam.consumer, nodeInput, building.node.input, thermal.building.service.input),
            region)

        hdcd_region_bld <- hdcd_region_annual %>%
          dplyr::left_join(hdcd_building,
                           by = c('region')) %>%
          # Remove columns with -ve hdcd/cooling and +ve/heating
          dplyr::filter(
            !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
            !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
          dplyr::select(-HDCD)


        # if (spatial %in% c('gcam_regions31_us52', 'gcam_us49')) {
        #
        #   hdcd_region_bld <- hdcd_region_annual %>%
        #     dplyr::left_join(dplyr::bind_rows(
        #       helios::L244.HDDCDD_building,
        #       unique(helios::L2441.HDDCDD_Fixed_gcamusa_seg %>%
        #                dplyr::mutate(thermal.building.service.input =
        #                                gsub("^(\\S*\\s+\\S+).*", "\\1", thermal.building.service.input)) %>%
        #                dplyr::select(-segment) %>%
        #                dplyr::rename(region = subRegion))),
        #                      by = c('region')) %>%
        #     # Remove columns with -ve hdcd/cooling and +ve/heating
        #     dplyr::filter(
        #       !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
        #       !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
        #     dplyr::select(-HDCD)
        #
        # }

      }


    } else if(model_timestep == 'daily') {

      #......................
      # Step 6b: Aggregate over monthly and annual
      #......................

      hdcd_region_monthly <- hdcd_region %>%
        dplyr::filter(!is.na(subRegion)) %>%
        # dplyr::select(-lat, -lon, -ID) %>%
        dplyr::mutate(month = lubridate::month(datetime)) %>%
        dplyr::group_by(region, subRegion, year, month) %>%
        dplyr::summarise(HD = sum(value[value < 0]),
                         CD = sum(value[value > 0])) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = c('HD', 'CD'), names_to = 'HDCD') %>%
        dplyr::filter(value != 0)

      hdcd_region_annual <- hdcd_region %>%
        dplyr::filter(!is.na(subRegion)) %>%
        # dplyr::select(-lat, -lon, -ID) %>%
        dplyr::group_by(region, subRegion, year) %>%
        dplyr::summarise(HD = sum(value[value < 0]),
                         CD = sum(value[value > 0])) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = c('HD', 'CD'), names_to = 'HDCD') %>%
        dplyr::filter(value != 0)

      #......................
      # Step 7b: Add in building components for GCAM
      #......................

      # basic structure of building thermal service
      hdcd_building <- data.frame(
        gcam.consumer = c('comm', 'comm', 'resid', 'resid'),
        nodeInput = c('comm', 'comm', 'resid', 'resid'),
        building.node.input = c('comm_building', 'comm_building', 'resid_building', 'resid_building'),
        thermal.building.service.input = c('comm cooling', 'comm heating', 'resid cooling', 'resid heating'))

      region <- unique(hdcd_region_annual$region)

      # expand building info to each region
      hdcd_building <- hdcd_building %>%
        tidyr::expand(
          tidyr::nesting(gcam.consumer, nodeInput, building.node.input, thermal.building.service.input),
          region)

      hdcd_region_bld <- hdcd_region_annual %>%
        dplyr::left_join(hdcd_building,
                         by = c('region')) %>%
        # Remove columns with -ve hdcd/cooling and +ve/heating
        dplyr::filter(
          !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
          !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
        dplyr::select(-HDCD)
    } # end of if(model_timestep == 'hourly')

  } else {
    print(paste0('None of the selected time_periods: ',
                 paste0(time_periods, collapse = ', ')))
    print(paste0('are available in the selected ncdf file chosen: ', ncdf_i))
  }


  #......................
  # Step 9a: Save segment data as combined csv files in Level 2 XML format for US or GCAM regions
  # L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv, L2441.HDDCDD_Fixed_rcp8p5_gcamusa.csv,
  # L2441.HDDCDD_Fixed_gcamusa.csv
  #......................

  if(nrow(hdcd_region_bld) > 0){

    if (model_timestep == 'hourly' & dispatch_segment == TRUE){
      hdcd_comb_gcam <- hdcd_comb_gcam %>%
        dplyr::bind_rows(hdcd_region_bld %>%
                           dplyr::mutate(unit = 'Fahrenheit degree-hours')) %>%
        dplyr::group_by(region, subRegion, year, segment, gcam.consumer,
                        nodeInput, building.node.input,
                        thermal.building.service.input, unit) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year),
                      !is.na(segment))

      hdcd_name <- 'hdhcdh'

      # for im3 analysis
      if(im3_analysis){

        hdcd_comb_gridregion_gcam <- hdcd_comb_gridregion_gcam %>%
          dplyr::bind_rows(hdcd_gridregion_bld %>%
                             dplyr::mutate(unit = 'Fahrenheit degree-hours')) %>%
          dplyr::group_by(region, subRegion, year, segment, gcam.consumer,
                          nodeInput, building.node.input,
                          thermal.building.service.input, unit) %>%
          dplyr::summarize(value = sum(value, na.rm = T)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(subRegion),
                        !is.na(year),
                        !is.na(segment))

      }

    } else if ((model_timestep == 'daily' & dispatch_segment == FALSE) |
               (model_timestep == 'hourly' & dispatch_segment == FALSE)){
      hdcd_comb_gcam <- hdcd_comb_gcam %>%
        dplyr::bind_rows(hdcd_region_bld %>%
                           dplyr::mutate(unit = 'Fahrenheit degree-days')) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(region, subRegion, year, gcam.consumer,
                        nodeInput, building.node.input,
                        thermal.building.service.input, unit) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year))

      hdcd_name <- 'hddcdd'

    } else {
      stop('Output by dispatch segment requires hourly climate data.')
    }

    if(i == length(ncdf)){

      year_min_i <- min(hdcd_comb_gcam$year, na.rm = T)
      year_max_i <- max(hdcd_comb_gcam$year, na.rm = T)

      filename_i <- file.path(
        folder,
        helios::create_name(c(hdcd_name, model, year_min_i, year_max_i, 'gcam',
                              gsub('_', '-', gsub('gcam_', '', spatial)), name_append), 'csv'))

      if(save){
        data.table::fwrite(hdcd_comb_gcam, file = filename_i)
        print(paste0('File saved as : ', filename_i))
      }
    }

    # for IM3 analysis
    if(im3_analysis){

      if(i == length(ncdf)){

        year_min_i <- min(hdcd_comb_gridregion_gcam$year, na.rm = T)
        year_max_i <- max(hdcd_comb_gridregion_gcam$year, na.rm = T)

        filename_i_gridregion <- file.path(
          folder,
          helios::create_name(c('hdhcdh', model, year_min_i, year_max_i, 'gcam', 'gridregion', name_append), 'csv'))

        if(save){
          data.table::fwrite(hdcd_comb_gridregion_gcam, file = filename_i_gridregion)
          print(paste0('File saved as : ', filename_i_gridregion))
        }
      }

    }

  }

  #......................
  # Step 9b: Save monthly as combined csv files
  #......................

  if(nrow(hdcd_region_monthly) > 0){

    hdcd_comb_monthly <- hdcd_comb_monthly %>%
      dplyr::bind_rows(hdcd_region_monthly %>%
                         dplyr::mutate(unit = 'Fahrenheit degree-days')) %>%
      dplyr::group_by(region, subRegion, year, month, HDCD, unit) %>%
      dplyr::summarize(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(subRegion),
                    !is.na(year),
                    !is.na(month))

    if(i == length(ncdf)){

      year_min_i <- min(hdcd_comb_monthly$year, na.rm = T)
      year_max_i <- max(hdcd_comb_monthly$year, na.rm = T)

      filename_i_monthly <- file.path(
        folder,
        helios::create_name(c('hddcdd', model, year_min_i, year_max_i, 'monthly',
                              gsub('_', '-', gsub('gcam_', '', spatial)), name_append), 'csv'))

      if(save){
        data.table::fwrite(hdcd_comb_monthly, file = filename_i_monthly)
        print(paste0('File saved as : ', filename_i_monthly))
      }
    }


    # for IM3 grid region analysis
    if(im3_analysis){

      hdcd_comb_gridregion_monthly <- hdcd_comb_gridregion_monthly %>%
        dplyr::bind_rows(hdcd_gridregion_monthly %>%
                           dplyr::mutate(unit = 'Fahrenheit degree-hours')) %>%
        dplyr::group_by(region, subRegion, year, month, HDCD, unit) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year),
                      !is.na(month))

      if(i == length(ncdf)){

        year_min_i <- min(hdcd_comb_gridregion_monthly$year, na.rm = T)
        year_max_i <- max(hdcd_comb_gridregion_monthly$year, na.rm = T)

        filename_i_gridregion_monthly <- file.path(
          folder,
          helios::create_name(c('hdhcdh', model, year_min_i, year_max_i, 'monthly', 'gridregion', name_append), 'csv'))

        if(save){
          data.table::fwrite(hdcd_comb_gridregion_monthly, file = filename_i_gridregion_monthly)
          print(paste0('File saved as : ', filename_i_gridregion_monthly))
        }
      }


    }

  }

  #......................
  # Step 9c: Save annual as combined csv files
  #......................

  if(nrow(hdcd_region_annual) > 0){

    hdcd_comb_annual <- hdcd_comb_annual %>%
      dplyr::bind_rows(hdcd_region_annual %>%
                         dplyr::mutate(unit = 'Fahrenheit degree-days')) %>%
      dplyr::group_by(region, subRegion, year, HDCD, unit) %>%
      dplyr::summarize(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(subRegion),
                    !is.na(year))

    if(i == length(ncdf)){

      year_min_i <- min(hdcd_comb_annual$year, na.rm = T)
      year_max_i <- max(hdcd_comb_annual$year, na.rm = T)

      filename_i_annual <- file.path(
        folder,
        helios::create_name(c('hddcdd', model, year_min_i, year_max_i, 'annual',
                              gsub('_', '-', gsub('gcam_', '', spatial)), name_append), 'csv'))

      if(save){
        data.table::fwrite(hdcd_comb_annual, file = filename_i_annual)
        print(paste0('File saved as : ', filename_i_annual))
      }
    }


    # for IM3 grid region analysis
    if(im3_analysis){

      hdcd_comb_gridregion_annual <- hdcd_comb_gridregion_annual %>%
        dplyr::bind_rows(hdcd_gridregion_annual %>%
                           dplyr::mutate(unit = 'Fahrenheit degree-hours')) %>%
        dplyr::group_by(region, subRegion, year, HDCD, unit) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year))

      if(i == length(ncdf)){

        year_min_i <- min(hdcd_comb_gridregion_annual$year, na.rm = T)
        year_max_i <- max(hdcd_comb_gridregion_annual$year, na.rm = T)

        filename_i_gridregion_annual <- file.path(
          folder,
          helios::create_name(c('hdhcdh', model, year_min_i, year_max_i, 'annual', 'gridregion', name_append), 'csv'))

        if(save){
          data.table::fwrite(hdcd_comb_gridregion_annual, file = filename_i_gridregion_annual)
          print(paste0('File saved as : ', filename_i_gridregion_annual))
        }
      }



    }

  }


  #......................
  # Step 9d: Save hourly HDCD as combined csv files
  #......................

  if(nrow(hdcd_region) > 0){

    hdcd_comb_hourly <- hdcd_region %>%
      dplyr::mutate(unit = 'Fahrenheit degree-hours')

    if(i == length(ncdf)){

      year_min_i <- min(hdcd_comb_hourly$year, na.rm = T)
      year_max_i <- max(hdcd_comb_hourly$year, na.rm = T)

      filename_i_hourly <- file.path(
        folder,
        helios::create_name(c('hdhcdh', model, year_min_i, year_max_i, 'hourly',
                              gsub('_', '-', gsub('gcam_', '', spatial)), name_append), 'csv'))

      if(save){
        data.table::fwrite(hdcd_comb_hourly, file = filename_i_hourly)
        print(paste0('File saved as : ', filename_i_hourly))
      }
    }


    # for IM3 grid region analysis
    if(im3_analysis){

      hdcd_comb_gridregion_hourly <- hdcd_gridregion %>%
        dplyr::mutate(unit = 'Fahrenheit degree-hours')

      if(i == length(ncdf)){

        year_min_i <- min(hdcd_comb_gridregion_hourly$year, na.rm = T)
        year_max_i <- max(hdcd_comb_gridregion_hourly$year, na.rm = T)

        filename_i_gridregion_hourly <- file.path(
          folder,
          helios::create_name(c('hdhcdh', model, year_min_i, year_max_i, 'hourly', 'gridregion', name_append), 'csv'))

        if(save){
          data.table::fwrite(hdcd_comb_gridregion_hourly, file = filename_i_gridregion_hourly)
          print(paste0('File saved as : ', filename_i_gridregion_hourly))
        }
      }

    }

  }

  #......................
  # Step 9e: Save segment mapping with auto-pick super peak
  #......................

  if(im3_analysis){

    # save us 49 segment mapping
    filename_segment_map <- file.path(
      folder,
      helios::create_name(c('segment_map', year_min_i, year_max_i,
                            gsub('_', '-', gsub('gcam_', '', spatial)), name_append), 'csv'))

    data.table::fwrite(segment_map_utc_region,
                       file = filename_segment_map)
    print(paste0('File saved as : ', filename_segment_map))


    # save grid region segement mapping
    filename_segment_map <- file.path(
      folder,
      helios::create_name(c('segment_map', year_min_i, year_max_i, 'gridregion', name_append), 'csv'))

    data.table::fwrite(segment_map_utc_gridregion,
                       file = filename_segment_map)
    print(paste0('File saved as : ', filename_segment_map))
  }

  #......................
  # Step 10: Save XML
  #......................

  if(xml){
    helios::save_xml(hdcd_gcam = hdcd_comb_gcam,
                     folder = folder,
                     filename = filename_i,
                     name_append = name_append)
    }


  #......................
  # Step 11: Diagnostics
  #......................

  if(diagnostics){

    if(im3_analysis & !is.null(elec_share)){

      helios::diagnostics(
        hdcd_segment = hdcd_comb_gcam,
        hdcd_monthly = hdcd_comb_monthly,
        segment_map = segment_map_utc_region,
        min_diagnostic_months = length(unique(hdcd_comb_monthly$month)),
        folder = folder,
        name_append = model)

      helios::diagnostics(
        hdcd_segment = hdcd_comb_gridregion_gcam,
        hdcd_monthly = hdcd_comb_gridregion_monthly,
        segment_map = segment_map_utc_gridregion,
        min_diagnostic_months = length(unique(hdcd_comb_monthly$month)),
        folder = folder,
        name_append = model)


    } else {

      helios::diagnostics(
        hdcd_segment = hdcd_comb_gcam,
        hdcd_monthly = hdcd_comb_monthly,
        segment_map = NULL,
        min_diagnostic_months = length(unique(hdcd_comb_monthly$month)),
        folder = folder,
        name_append = model)

    }


  }


  #......................
  # Close out
  #......................

  print('process_hdcd completed.')

  # return data
  invisible(list(hdcd_comb_gcam = hdcd_comb_gcam,
                 hdcd_comb_monthly = hdcd_comb_monthly,
                 hdcd_comb_annual = hdcd_comb_annual,
                 hdcd_comb_gridregion_gcam = hdcd_comb_gridregion_gcam,
                 hdcd_comb_gridregion_monthly = hdcd_comb_gridregion_monthly,
                 hdcd_comb_gridregion_annual = hdcd_comb_gridregion_annual))

} # Close process_hdcd

