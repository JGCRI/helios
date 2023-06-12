#' hdcd
#'
#' Heating and Cooling Degree processing for GCAM from various sources such as WRF and CMIP
#'
#' @param ncdf Default = NULL. Path to ncdf file.
#' @param ncdf_var Default = NULL. Variable to extract from NetCDF file.
#' @param model Default = NULL. Climate model that generates the ncdf file. Options: 'wrf' or 'cmip'.
#' @param model_timestep Default = NULL. String for time step of input climate data. Options: 'hourly' or 'daily'
#' @param population Default = NULL. Path to population files to population weight data.
#' @param spatial Default = NULL. Options: check helios::spatial_options. 'gcam_us49', 'gcam_regions32', 'gcam_regions31_us52'. Aggregate to different spatial boundaries.
#' @param time_periods Default = NULL. integer vector. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @param dispatch_segment Default = FALSE. TRUE if want to output degree-hours by GCAM-USA dispatch segment. This can only be TRUE when model time_step is set to 'hourly'.
#' @param reference_temp_F Default = 65
#' @param folder Default = paste0(getwd(),'/output').
#' @param diagnostics Default = F.
#' @param xml Default = F. Whether to create GCAM XML or not.
#' @param save Default = T. Whether to save outputs or not.
#' @param name_append Default = ''. Name to append to all filenames
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
                 save = T) {

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
    hdcd_region_bld <- tibble::tibble()
    hdcd_region_monthly <- tibble::tibble()
    hdcd_region_annual<- tibble::tibble()

    # check population input format
    if (any(grepl('tbl_df|tbl|data.frame', class(population)))) {
      population <- list('pop' = population)
    }

    # check if there is valid model_step input
    if(any(is.null(model_timestep), !model_timestep %in% c('hourly', 'daily'))){
      stop('Please provide model time step. Options: daily or hourly')
    }

    # check if there is valid spatial input
    if(any(is.null(spatial), !spatial %in% helios::spatial_options$spatial)){
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

    if(file.exists(ncdf_i)){

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

        ncdf_grid <- helios::read_ncdf(ncdf = ncdf_i,
                                       model = model,
                                       var = ncdf_var,
                                       time_periods = time_periods)

        # find region and subRegion info based on data grid lat lon
        ncdf_grid <- helios::find_mapping_grid(data = ncdf_grid,
                                               spatial = spatial)

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

          # read population based on the population data type
          # output from wrf resolution: ['ID', 'subRegion', 'lat', 'lon', 'year', 'value' ]
          # output from 1/8th degree pop data: ['ID', 'subRegion', 'lat', 'lon', 'year', 'value' ]
          population_j_grid <- helios::read_population(file = population_j,
                                                       time_periods = time_periods)

          population_j_grid <- helios::match_grids(from_df = population_j_grid,
                                                   to_df = ncdf_pivot,
                                                   time_periods = time_periods)

          grid_intersect <- ncdf_grid %>%
            dplyr::select(lon, lat) %>%
            dplyr::inner_join(population_j_grid %>% dplyr::select(lon, lat),
                              by = c('lon', 'lat'))
          # check if population's grid matches climate data's grids
          if(nrow(grid_intersect) == 0) {
            stop('Climate and population data has different resolutions.')
          }

          population_j_grid <- helios::find_mapping_grid(data = population_j_grid,
                                                         spatial = spatial)




          # Create population weighted raster if any population years in ncdf years
          if (any(unique(population_j_grid$year) %in% years)) {

            # Weighted population tibble
            print('Starting population weighting ...')

            population_j_weighted <- population_j_grid %>%
              dplyr::group_by(region, ID, subRegion, year) %>%
              dplyr::mutate(subRegion_total_value = sum(value, na.rm = T)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(pop_weight = value / subRegion_total_value)

            print('Completed population weighting.')

            #......................
            # Step 3: Calculate Heating and Cooling Degrees (Kelvin to F)
            # Step 4: Population weight for each grid for each year
            #......................


            ncdf_hdcd_pop_weighted <- ncdf_pivot %>%
              dplyr::left_join(population_j_weighted %>%
                                 dplyr::select(-value, -subRegion_total_value),
                               by = c('ID', 'region', 'subRegion', 'lat', 'lon', 'year')) %>%
              dplyr::mutate(value = (((value - 273.15) * 9/5) + 32) - reference_temp_F,
                            value = dplyr::if_else(is.na(pop_weight), value, value * pop_weight))

          } else {
            print(paste0('Population data years: ', paste(names(population_j_grid)[!grepl('RID|lat|lon', names(population_j_grid))], collapse = ',')))
            print(paste0('ncdf data years: ', as.character(years)))
            stop('Population data provided does not contain data for any of the years in the ncdf data.')
          }
        } else {
          stop('Please provide valide population file path.')
        }

        #......................
        # Step 5: Subset for time periods chosen
        #......................

        # Subset raster brick to selected times
        if (length(index_subset) > 0) {

          if (model_timestep == 'hourly') {

            # Equivalent to step 6: Aggregate to regions
            hdcd_region <- ncdf_hdcd_pop_weighted %>%
              dplyr::filter(!is.na(subRegion)) %>%
              dplyr::select(-lat, -lon) %>%
              dplyr::group_by(region, subRegion, ID, year, datetime) %>%
              dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
              dplyr::ungroup()

            # Assign HDCD categories
            hdcd_region <- hdcd_region %>%
              dplyr::mutate(HDCD = dplyr::if_else(value < 0, 'HD', 'CD')) %>%
              dplyr::filter(value != 0)

            #......................
            # Step 6a: Aggregate over Segments, monthly and annual
            #......................

            temporal_subset <- data.frame(ncdf_times = ncdf_times[index_subset],
                                          x = paste0('X', index_subset)) %>%
              dplyr::mutate(datetime = as.POSIXct(ncdf_times, format = '%Y-%m-%d_%H:%M:%S', tz = 'UTC')) %>%
              dplyr::mutate(year = lubridate::year(datetime),
                            month = lubridate::month(datetime),
                            day = lubridate::day(datetime),
                            hour = lubridate::hour(datetime),
                            timezone = lubridate::tz(datetime)) %>%
              dplyr::mutate(month = dplyr::if_else(month < 10, paste0('0', month), paste0(month)),
                            day = dplyr::if_else(day < 10, paste0('0', day), paste0(day)),
                            hour = dplyr::if_else(hour < 10, paste0('0', hour), paste0(hour)) )

            # hdcd segments
            hdcd_region_segments <- hdcd_region %>%
              dplyr::left_join(temporal_subset, by = c('datetime', 'year')) %>%
              dplyr::left_join(helios::segment_map_utc,
                               by = c('subRegion', 'month', 'day', 'hour')) %>%
              dplyr::group_by(region, subRegion, year, segment, HDCD) %>%
              dplyr::summarise(value = sum(value, na.rm = T))

            # hdcd monthly
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

            # hdcd annual
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

              } else {
                # only gcam_us49 can have dispatch segment because
                # we don't have day, night segement for other regions
                stop(paste0('Dispatch segments cannot be applied to: ', spatial))

              }

            } else {

              if(spatial =='gcam_regions32'){

                hdcd_region_bld <- hdcd_region_annual %>%
                  dplyr::left_join(helios::L244.HDDCDD_building,
                                   by = c('region')) %>%
                  # Remove columns with -ve hdcd/cooling and +ve/heating
                  dplyr::filter(
                    !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
                    !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
                  dplyr::select(-HDCD)

              }

              if (spatial %in% c('gcam_regions31_us52', 'gcam_us49')) {

                hdcd_region_bld <- hdcd_region_annual %>%
                  dplyr::left_join(dplyr::bind_rows(
                    helios::L244.HDDCDD_building,
                    unique(helios::L2441.HDDCDD_Fixed_gcamusa_seg %>%
                             dplyr::mutate(thermal.building.service.input =
                                             gsub("^(\\S*\\s+\\S+).*", "\\1", thermal.building.service.input)) %>%
                             dplyr::select(-segment) %>%
                             dplyr::rename(region = subRegion))),
                                   by = c('region')) %>%
                  # Remove columns with -ve hdcd/cooling and +ve/heating
                  dplyr::filter(
                    !((value < 0) & grepl('cool', thermal.building.service.input, ignore.case = T)),
                    !((value > 0) & grepl('heat', thermal.building.service.input, ignore.case = T))) %>%
                  dplyr::select(-HDCD)

              }

            }


          } else if(model_timestep == 'daily') {

            #......................
            # Step 6b: Aggregate over monthly and annual
            #......................

            hdcd_region_monthly <- ncdf_hdcd_pop_weighted %>%
              dplyr::filter(!is.na(subRegion)) %>%
              dplyr::select(-lat, -lon, -ID) %>%
              dplyr::mutate(month = lubridate::month(datetime)) %>%
              dplyr::group_by(region, subRegion, year, month) %>%
              dplyr::summarise(HD = sum(value[value < 0]),
                               CD = sum(value[value > 0])) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_longer(cols = c('HD', 'CD'), names_to = 'HDCD') %>%
              dplyr::filter(value != 0)

            hdcd_region_annual <- ncdf_hdcd_pop_weighted %>%
              dplyr::filter(!is.na(subRegion)) %>%
              dplyr::select(-lat, -lon, -ID) %>%
              dplyr::group_by(region, subRegion, year) %>%
              dplyr::summarise(HD = sum(value[value < 0]),
                               CD = sum(value[value > 0])) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_longer(cols = c('HD', 'CD'), names_to = 'HDCD') %>%
              dplyr::filter(value != 0)

            #......................
            # Step 7b: Add in building components for GCAM
            #......................

            hdcd_region_bld <- hdcd_region_annual %>%
              dplyr::left_join(helios::L244.HDDCDD_building,
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

          print(paste0('Processing hdcd completed for file: ', ncdf_i))

      } # end of for(j in 1:length(population))


    } else { # Close if(file.exists(ncdf_i)){
      print(paste0('Skipping hdcd for file which does not exist: ', ncdf_i))
    } # end of if(file.exists(ncdf_i))

    #......................
    # Step 9a: Save segment data as combined csv files in Level 2 XML format for US or GCAM regions
    # L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv, L2441.HDDCDD_Fixed_rcp8p5_gcamusa.csv,
    # L2441.HDDCDD_Fixed_gcamusa.csv
    #......................

    if(nrow(hdcd_region_bld) > 0){

      if (model_timestep == 'hourly' & dispatch_segment == TRUE){
        hdcd_comb_gcam <- hdcd_comb_gcam %>%
          dplyr::bind_rows(hdcd_region_bld %>%
                             dplyr::mutate(unit = 'degree-hours')) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(region, subRegion, year, segment, gcam.consumer,
                          nodeInput, building.node.input,
                          thermal.building.service.input, unit) %>%
          dplyr::summarize(value = sum(value, na.rm = T)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(subRegion),
                        !is.na(year),
                        !is.na(segment))
      } else if ((model_timestep == 'daily' & dispatch_segment == FALSE) |
                 (model_timestep == 'hourly' & dispatch_segment == FALSE)){
        hdcd_comb_gcam <- hdcd_comb_gcam %>%
          dplyr::bind_rows(hdcd_region_bld %>%
                             dplyr::mutate(unit = 'degree-days')) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(region, subRegion, year, gcam.consumer,
                          nodeInput, building.node.input,
                          thermal.building.service.input, unit) %>%
          dplyr::summarize(value = sum(value, na.rm = T)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(subRegion),
                        !is.na(year))
      } else {
        stop('Output by dispatch segment requires hourly climate data.')
      }


      year_min_i <- min(hdcd_comb_gcam$year, na.rm = T)
      year_max_i <- max(hdcd_comb_gcam$year, na.rm = T)

      if(i < length(ncdf)){
        filename_i <- file.path(
          folder,
          helios::create_name(c('hdcd', model, 'intermediate_gcam', name_append), 'csv'))
        } else {
          filename_i <- file.path(
            folder,
            helios::create_name(c('hdcd', model, year_min_i, year_max_i, 'gcam', name_append), 'csv'))
        }

      if(save){
        data.table::fwrite(hdcd_comb_gcam, file = filename_i)
        print(paste0('File saved as : ', filename_i))
        }
      }

    #......................
    # Step 9b: Save monthly as combined csv files
    #......................

    if(nrow(hdcd_region_monthly) > 0){

      hdcd_comb_monthly <- hdcd_comb_monthly %>%
        dplyr::bind_rows(hdcd_region_monthly %>%
                           dplyr::mutate(unit = 'degree-days')) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(region, subRegion, year, month, HDCD, unit) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year),
                      !is.na(month))

      year_min_i <- min(hdcd_comb_monthly$year, na.rm = T)
      year_max_i <- max(hdcd_comb_monthly$year, na.rm = T)

      if(i < length(ncdf)){
        filename_i_monthly <- file.path(
          folder,
          helios::create_name(c('hdcd', model, 'intermediate_monthly', name_append), 'csv'))
        } else {
        filename_i_monthly <- file.path(
          folder,
          helios::create_name(c('hdcd', model, year_min_i, year_max_i, 'monthly', name_append), 'csv'))
        }

      if(save){
        data.table::fwrite(hdcd_comb_monthly, file = filename_i_monthly)
        print(paste0('File saved as : ', filename_i_monthly))
      }

    }

    #......................
    # Step 9c: Save annual as combined csv files
    #......................

    if(nrow(hdcd_region_annual) > 0){

      hdcd_comb_annual <- hdcd_comb_annual %>%
        dplyr::bind_rows(hdcd_region_annual %>%
                           dplyr::mutate(unit = 'degree-days')) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(region, subRegion, year, HDCD, unit) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year))

      year_min_i <- min(hdcd_comb_annual$year, na.rm = T)
      year_max_i <- max(hdcd_comb_annual$year, na.rm = T)

      if(i < length(ncdf)){
        filename_i_annual <- file.path(
          folder,
          helios::create_name(c('hdcd', model, 'intermediate_annual', name_append), 'csv'))
        } else {
        filename_i_annual <- file.path(
          folder,
          helios::create_name(c('hdcd', model, year_min_i, year_max_i, 'annual', name_append), 'csv'))
        }

      if(save){
        data.table::fwrite(hdcd_comb_annual, file = filename_i_annual)
        print(paste0('File saved as : ', filename_i_annual))
      }

    }

  } # Close for(i in 1:length(ncdf)){


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

    helios::diagnostics(
      hdcd_segment = hdcd_comb_gcam,
      hdcd_monthly = hdcd_comb_monthly,
      min_diagnostic_months = length(unique(hdcd_comb_monthly$month)),
      folder = folder,
      name_append = model)
  }


  #......................
  # Close out
  #......................

  print('process_hdcd completed.')

  # return data
  invisible(list(hdcd_comb_gcam = hdcd_comb_gcam,
                 hdcd_comb_monthly = hdcd_comb_monthly,
                 hdcd_comb_annual = hdcd_comb_annual))

} # Close process_hdcd

