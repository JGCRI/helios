#' hdcd
#'
#' Heating and Cooling Degree processing for GCAM from various sources such as WRF and CMIP
#'
#' @param ncdf Default = NULL. Path to ncdf file.
#' @param ncdf_var Default = NULL. Variable to extract from NetCDF file.
#' @param model Default = NULL. Climate model that generates the ncdf file. Options: 'wrf' or 'cmip'
#' @param population Default = NULL. Path to population files to population weight data.
#' @param spatial Default = NULL. Options: "states_us_49", "gcam_regions_32". Aggregate to different spatial boundaries.
#' @param temporal Default = NULL. integer vector. If not specified, set to GCAM periods seq(2020, 2100, 5).
#' @param reference_temp_F Default = 65
#' @param folder Default = paste0(getwd(),"/output").
#' @param diagnostics Default = F.
#' @param xml Default = F. Whether to create GCAM XML or not.
#' @param save Default = T. Whether to save outputs or not.
#' @param name_append Default = "". Name to append to all filenames
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

hdcd <- function(ncdf = NULL,
                 ncdf_var = NULL,
                 model = NULL,
                 population = NULL,
                 spatial = NULL,
                 temporal = NULL,
                 reference_temp_F = 65,
                 folder = paste0(getwd(),"/output"),
                 diagnostics = F,
                 xml = F,
                 name_append = "",
                 save = T) {

  print("Starting function process_hdcd...")

  #......................
  # Initialize
  #......................

  if(T){

    NULL -> RID -> subRegion_total_value -> pop_weight -> stateCode ->
      HDDCDD -> scenario -> scenario_hddcdd -> ID -> V3 -> day ->
      building.node.input -> gcam.consumer -> heatcool -> month -> nodeInput ->
      segment -> subRegion -> thermal.building.service.input -> value ->
      x -> y -> year

    if(is.null(folder)) {
      folder <- paste0(getwd(), "/output")
    }

    if (save | diagnostics) {
      if (!dir.exists(folder)) {
        dir.create(folder)
      }
    }

    # Pick up on intermediate files if program crashed before.
    hdcd_comb <- tibble::tibble()
    hdcd_comb_monthly <- tibble::tibble()
    hdcd_comb_annual <- tibble::tibble()
    hdcd_region_bld <- tibble::tibble()
    hdcd_region_monthly <- tibble::tibble()
    hdcd_region_annual<- tibble::tibble()

    if (any(grepl("tbl_df|tbl|data.frame", class(population)))) {
      population <- list("pop" = population)
    }
  }

  #......................
  # Loop over each ncdf file
  #......................

  print(paste0("Processing files provided: ", paste0(ncdf, collapse = ", ")))

  for(i in 1:length(ncdf)){

    ncdf_i <- ncdf[i]

    if(file.exists(ncdf_i)){

      for(j in 1:length(population)){

        #......................
        # Step 1: Process Temperature netCDF file
        #......................
        # Assign time_periods
        if (is.null(temporal)) {

          time_periods <- seq(2020, 2100, by = 5)

        } else {

          if (all(temporal == floor(temporal))) {

            time_periods <- temporal

          } else {
            stop('Please provide valid vector. For example, c(2020, 2030).')
          }

        }

        print(".........................................")
        print(paste0("Running hdcd for file: ", ncdf_i))

        ncdf_grid <- helios::read_ncdf(ncdf = ncdf_i,
                                       model = model,
                                       var = ncdf_var,
                                       time_periods = time_periods)

        ncdf_times <- names(ncdf_grid)[
          !names(ncdf_grid) %in% c('lat', 'lon', 'region', 'subRegion', 'ID')]


        indices <- as.integer(grepl(paste0(time_periods, collapse = "|"), ncdf_times))
        ncdf_times_subset <- ncdf_times[grepl(paste0(time_periods, collapse = "|"), ncdf_times)]
        index_subset <- c(1:length(ncdf_times)) * indices
        index_subset <- index_subset[!index_subset %in% 0]
        years <- unique(substr(ncdf_times, 1, 4))

        #......................
        # Step 2: Population weighted grid
        #......................

        if (!is.null(population)) {

          population_j = population[[j]]

          # read population based on the population data type
          # output from wrf resolution: ["ID", "subRegion", "lat", "lon", "year", "value" ]
          # output from 1/8th degree pop data: ["ID", "subRegion", "lat", "lon", "year", "value" ]
          population_j_grid <- helios::read_population(file = population_j)

          # Create population weighted raster if any population years in ncdf years
          if (any(unique(population_j_grid$year) %in% as.character(years))) {

            # Weighted population tibble
            print("Starting population weighting ...")

            population_j_weighted <- population_j_grid %>%
              dplyr::group_by(region, ID, subRegion, year) %>%
              dplyr::mutate(subRegion_total_value = sum(value, na.rm = T)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(pop_weight = value / subRegion_total_value)

            print("Completed population weighting.")

            #......................
            # Step 3: Calculate Heating and Cooling Degrees (Kelvin to F)
            # Step 4: Population weight for each grid for each year
            #......................
            if (model == 'wrf') {

              ncdf_pivot <- ncdf_grid %>%
                tidyr::pivot_longer(cols = ncdf_times, names_to = 'datetime') %>%
                dplyr::mutate(datetime = as.POSIXct(datetime,
                                                    format = "%Y-%m-%d_%H:%M:%S",
                                                    tz = "UTC")) %>%
                dplyr::mutate(year = as.character(lubridate::year(datetime)))

            } else if (model == 'cmip') {

              ncdf_pivot <- ncdf_grid %>%
                tidyr::pivot_longer(cols = ncdf_times, names_to = 'datetime') %>%
                dplyr::mutate(datetime = as.POSIXct(datetime,
                                                    format = "%Y-%m-%d",
                                                    tz = "UTC")) %>%
                dplyr::mutate(year = as.character(lubridate::year(datetime)))

            } else {

              stop('Please select valid model. Options: wrf, cmip')

            }

            ncdf_hdcd_pop_weighted <- ncdf_pivot %>%
              dplyr::left_join(population_j_weighted %>%
                                 dplyr::select(-value, -subRegion_total_value),
                               by = c('ID', 'subRegion', 'lat', 'lon', 'year')) %>%
              dplyr::mutate(value = (((value - 273.15) * 9/5) + 32) - reference_temp_F,
                            value = dplyr::if_else(is.na(pop_weight), value, value * pop_weight))

          } else {
            print(paste0("Population data years: ", paste(names(population_j_grid)[!grepl("RID|lat|lon", names(population_j_grid))], collapse = ",")))
            print(paste0("ncdf data years: ", as.character(years)))
            stop("Population data provided does not contain data for any of the years in the ncdf data.")
          }
        }

        #......................
        # Step 5: Subset for time periods chosen
        #......................

        if (spatial == 'states_us_49') {

          # Subset raster brick to selected times
          if (length(index_subset) > 0) {
            # Equivalent to step 6: Aggregate to regions
            hdcd_region <- ncdf_hdcd_pop_weighted %>%
              dplyr::filter(!is.na(subRegion)) %>%
              dplyr::select(-lat, -lon) %>%
              dplyr::group_by(subRegion, ID, year, datetime) %>%
              dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
              dplyr::ungroup()

            # Assign HDDCDD categories
            hdcd_region <- hdcd_region %>%
              dplyr::mutate(HDDCDD = dplyr::if_else(value < 0, "HDD", "CDD")) %>%
              dplyr::filter(value != 0)

            #......................
            # Step 6a: Aggregate over Segments, monthly and annual
            #......................

            temporal_subset <- data.frame(ncdf_times = ncdf_times[index_subset],
                                          x = paste0("X", index_subset)) %>%
              dplyr::mutate(datetime = as.POSIXct(ncdf_times, format = "%Y-%m-%d_%H:%M:%S", tz = "UTC")) %>%
              dplyr::mutate(year = lubridate::year(datetime),
                            month = lubridate::month(datetime),
                            day = lubridate::day(datetime),
                            hour = lubridate::hour(datetime),
                            timezone = lubridate::tz(datetime)) %>%
              dplyr::mutate(year = as.character(year),
                            month = dplyr::if_else(month < 10, paste0("0", month), paste0(month)),
                            day = dplyr::if_else(day < 10, paste0("0", day), paste0(day)),
                            hour = dplyr::if_else(hour < 10, paste0("0", hour), paste0(hour)) )

            # hdcd segments
            hdcd_region_segments <- hdcd_region %>%
              dplyr::left_join(temporal_subset, by = c("datetime", "year")) %>%
              dplyr::left_join(helios::segment_map_utc,
                               by = c("subRegion", "month", "day", "hour")) %>%
              dplyr::group_by(subRegion, year, segment, HDDCDD) %>%
              dplyr::summarise(value = sum(value, na.rm = T))

            # hdcd monthly
            hdcd_region_monthly <- hdcd_region %>%
              dplyr::left_join(temporal_subset, by = c("datetime", "year")) %>%
              dplyr::group_by(subRegion, year, month, day) %>%
              dplyr::summarise(value = (max(value) + min(value)) / 2) %>%
              dplyr::ungroup() %>%
              dplyr::group_by(subRegion, year, month) %>%
              dplyr::summarise(HDD = sum(value[value < 0]),
                               CDD = sum(value[value > 0])) %>%
              dplyr::ungroup() %>%
              tidyr::gather(key = 'HDDCDD', value = 'value', HDD, CDD)

            # hdcd annual
            hdcd_region_annual <- hdcd_region %>%
              dplyr::left_join(temporal_subset, by = c("datetime", "year")) %>%
              dplyr::group_by(subRegion, year, month, day) %>%
              dplyr::summarise(value = (max(value) + min(value)) / 2) %>%
              dplyr::ungroup() %>%
              dplyr::group_by(subRegion, year) %>%
              dplyr::summarise(HDD = sum(value[value < 0]),
                               CDD = sum(value[value > 0])) %>%
              dplyr::ungroup() %>%
              tidyr::gather(key = 'HDDCDD', value = 'value', HDD, CDD)
          } else {
            print(paste0("None of the selected time_periods: ",
                         paste0(time_periods, collapse = ", ")))
            print(paste0("are available in the selected ncdf file chosen: ", ncdf_i))
          }

          #......................
          # Step 7a: Add in building components for GCAMUSA
          #......................

          hdcd_region_bld <- hdcd_region_segments %>%
            dplyr::left_join(helios::L2441.HDDCDD_Fixed_gcamusa_seg,
                             by = c("subRegion", "segment")) %>%
            # Remove columns with -ve hdcd/cooling and +ve/heating
            dplyr::filter(
              !((value < 0) & grepl("cool", thermal.building.service.input, ignore.case = T)),
              !((value > 0) & grepl("heat", thermal.building.service.input, ignore.case = T))) %>%
            dplyr::select(-HDDCDD)

        }
        else if(spatial == 'gcam_region_32') {

          #......................
          # Step 6b: Aggregate over monthly and annual
          #......................

          hdcd_region_monthly <- ncdf_hdcd_pop_weighted %>%
            dplyr::filter(!is.na(subRegion)) %>%
            dplyr::select(-lat, -lon, -region, -ID) %>%
            dplyr::mutate(month = lubricate::month(datetime)) %>%
            dplyr::group_by(subRegion, year, month) %>%
            dplyr::summarise(HDD = sum(value[value < 0]),
                             CDD = sum(value[value > 0])) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(cols = c('HDD', 'CDD'), names_to = 'HDDCDD') %>%
            dplyr::filter(value != 0) %>%
            dplyr::rename(region = subRegion)

          hdcd_region_annual <- ncdf_hdcd_pop_weighted %>%
            dplyr::filter(!is.na(subRegion)) %>%
            dplyr::select(-lat, -lon, -region, -ID) %>%
            dplyr::group_by(subRegion, year) %>%
            dplyr::summarise(HDD = sum(value[value < 0]),
                             CDD = sum(value[value > 0])) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(cols = c('HDD', 'CDD'), names_to = 'HDDCDD') %>%
            dplyr::filter(value != 0) %>%
            dplyr::rename(region = subRegion)

          #......................
          # Step 7b: Add in building components for GCAM
          #......................

          hdcd_region_bld <- hdcd_region_annual %>%
            dplyr::left_join(helios::L244.HDDCDD_building,
                             by = c('region')) %>%
            # Remove columns with -ve hdcd/cooling and +ve/heating
            dplyr::filter(
              !((value < 0) & grepl("cool", thermal.building.service.input, ignore.case = T)),
              !((value > 0) & grepl("heat", thermal.building.service.input, ignore.case = T))) %>%
            dplyr::select(-HDDCDD)
        }
        else {
          stop('Please provide a valid spatial scale. Options: states_us_49, gcam_region_32.')
        }

          print(paste0("Processing hdcd completed for file: ", ncdf_i))

      }


    } else { # Close if(file.exists(ncdf_i)){
      print(paste0("Skipping hdcd for file which does not exist: ", ncdf_i))
    }

    #......................
    # Step 9a: Save segment data as combined csv files in Level 2 XML format for US or GCAM regions
    # L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv, L2441.HDDCDD_Fixed_rcp8p5_gcamusa.csv,
    # L2441.HDDCDD_Fixed_gcamusa.csv
    #......................

    if(nrow(hdcd_region_bld)>0){
      hdcd_comb <- hdcd_comb %>%
        dplyr::bind_rows(hdcd_region_bld) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(subRegion, year, segment, gcam.consumer,
                        nodeInput, building.node.input,
                        thermal.building.service.input) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year),
                      !is.na(segment))

      year_min_i <- min(hdcd_comb$year, na.rm = T)
      year_max_i <- max(hdcd_comb$year, na.rm = T)

      if(i < length(ncdf)){
        filename_i <- file.path(folder,
                                paste0('hdcd_', model, '_to_gcam_intermediate',
                                       name_append, ".csv"))
        } else {
          filename_i <- file.path(folder,
                                  paste0('hdcd_', model, '_to_gcam_',
                                         year_min_i, '_', year_max_i,
                                         name_append, ".csv"))
          }

      if(save){
        data.table::fwrite(hdcd_comb, file = filename_i)
        print(paste0("File saved as : ", filename_i))
        }
      }

    #......................
    # Step 9b: Save monthly as combined csv files
    #......................

    if(nrow(hdcd_region_monthly)>0){

      hdcd_comb_monthly <- hdcd_comb_monthly %>%
        dplyr::bind_rows(hdcd_region_monthly) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(subRegion, year, month, HDDCDD) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year),
                      !is.na(month))

      year_min_i <- min(hdcd_comb_monthly$year, na.rm = T)
      year_max_i <- max(hdcd_comb_monthly$year, na.rm = T)

      if(i < length(ncdf)){
        filename_i_monthly <- file.path(folder,
                                        paste0('hdcd_', model, '_to_gcam_intermediate_monthly',
                                               name_append, ".csv"))
        } else {
        filename_i_monthly <- file.path(folder,
                                        paste0('hdcd_', model, '_to_gcam_',
                                               year_min_i, '_', year_max_i,
                                               '_monthly', name_append, ".csv"))
        }

      if(save){
        data.table::fwrite(hdcd_comb_monthly, file = filename_i_monthly)
        print(paste0("File saved as : ", filename_i_monthly))
      }

    }

    #......................
    # Step 9c: Save annual as combined csv files
    #......................

    if(nrow(hdcd_region_annual)>0){

      hdcd_comb_annual <- hdcd_comb_annual %>%
        dplyr::bind_rows(hdcd_region_annual) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(subRegion, year, HDDCDD) %>%
        dplyr::summarize(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year))

      year_min_i <- min(hdcd_comb_annual$year, na.rm = T)
      year_max_i <- max(hdcd_comb_annual$year, na.rm = T)

      if(i < length(ncdf)){
        filename_i_annual <- file.path(folder,
                                       paste0('hdcd_', model, '_to_gcam_intermediate_annual',
                                              name_append, ".csv"))
        } else {
        filename_i_annual <- file.path(folder,
                                       paste0('hdcd_', model, '_to_gcam_',
                                              year_min_i, '_', year_max_i,
                                              '_mannual', name_append, ".csv"))
        }

      if(save){
        data.table::fwrite(hdcd_comb_annual, file=filename_i_annual)
        print(paste0("File saved as : ", filename_i_annual))
      }

    }

  } # Close for(i in 1:length(ncdf)){


  #......................
  # Step 10: Save XML
  #......................

  if(xml){
    helios::save_xml(hdcd = hdcd_comb,
                     folder = folder,
                     filename = filename_i,
                     name_append = name_append)
    }


  #......................
  # Step 11: Diagnostics
  #......................

  if(diagnostics){

    helios::diagnostics(
      hdcd = hdcd_comb,
      hdcd_monthly = hdcd_comb_monthly,
      folder = folder,
      filename = filename_i,
      name_append = name_append)
  }


  #......................
  # Close out
  #......................

  print("process_hdcd completed.")

  # return data
  invisible(list(hdcd_comb = hdcd_comb,
                 hdcd_comb_monthly = hdcd_comb_monthly,
                 hdcd_comb_annual = hdcd_comb_annual))

} # Close process_hdcd

