#' hdcd
#'
#' Heating and Cooling Degree processing for GCAM from various sources such as WRF and CMIP
#'
#' @param ncdf Default = NULL. Path to ncdf file.
#' @param spatial Default = NULL. Options: "gcamusa". Aggregate to different spatial boundaries.
#' @param temporal Default = NULL. Options: "gcamusa". Aggregate to timesteps.
#' @param population Default = NULL. Path to population files to population weight data.
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
                 spatial = NULL,
                 temporal = NULL,
                 population = NULL,
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
      building.node.input->gcam.consumer->heatcool->month->nodeInput->
      segment->subRegion->thermal.building.service.input->value->x->y->year

  if(is.null(folder)){folder <- paste0(getwd(),"/output")}
  if(save | diagnostics){if(!dir.exists(folder)){dir.create(folder)}}
  # Pick up on intermediate files if program crashed before.
  hdcd_comb <- tibble::tibble()
  hdcd_comb_monthly <- tibble::tibble()
  hdcd_region_segments_bld <- tibble::tibble()
  hdcd_region_monthly <- tibble::tibble()
  if(any(grepl("tbl_df|tbl|data.frame",class(population)))){population <- list("pop"=population)}
  }

  #......................
  # Loop over each ncdf file
  #......................

  print(paste0("Processing files provided: ",paste0(ncdf,collapse=", ")))

  for(i in 1:length(ncdf)){

    ncdf_i <- ncdf[i]

    if(file.exists(ncdf_i)){

      for(j in 1:length(population)){

        population_j = population[[j]]

        if(!is.null(population)) {

        # If isn't a dataframe check if file exists
        if(length(population_j) == 1){
          if(any(class(population_j) == "character")){
            if(file.exists(population_j)){
              population_j_raw = data.table::fread(population_j)
            } else {
              print(paste0("Population file provided: ",population_j," does not exist."))
              population_j_raw = "No population"
            }
          }} else if(any(grepl("tbl_df|tbl|data.frame",class(population_j)))){
            population_j_raw = population_j
          }

        # Rename latitude and longitude if needed
        if(!any(grepl("\\<latitude\\>",names(population_j_raw),ignore.case = T))){}else{
          population_j_raw <- population_j_raw %>% dplyr::rename(!!"lat" := (names(population_j_raw)[grepl("\\<latitude\\>",names(population_j_raw),ignore.case = T)])[1])}
        if(!any(grepl("\\<longitude\\>",names(population_j_raw),ignore.case = T))){}else{
          population_j_raw <- population_j_raw %>% dplyr::rename(!!"lon" := (names(population_j_raw)[grepl("\\<longitude\\>",names(population_j_raw),ignore.case = T)])[1])}

        }

      print(".........................................")
      print(paste0("Running hdcd for file: ", ncdf_i))

      #......................
      # Check Inputs
      #......................

      ncdf_in <- ncdf4::nc_open(ncdf_i)
      ncdf_brick <- raster::brick(ncdf_i,varname="T2",ncdf=TRUE)

      # Index of Times available
      ncdf_times <-  ncdf4::ncvar_get(ncdf_in,"Times")
      # Assign time_periods
      if(is.null(temporal)){time_periods = ncdf_times} else {
        if(temporal=="gcamusa"){time_periods = seq(2020,2100,by=5)}
      }
      years = unique(substr(ncdf_times,1,4))

      #......................
      # Step 1: Map grid (lat/lon) to each shape in the polygons being mapped to
      #......................

      # Base raster
      ncdf_ras <- ncdf_brick[[1]]

      # Lat and Lon from ncdf
      ncdf_lat <- (raster::brick(ncdf_i, varname = 'XLAT', ncdf = TRUE))[[1]]
      ncdf_lon <- (raster::brick(ncdf_i, varname = 'XLONG', ncdf = TRUE))[[1]]

      # Get Lat long
      ncdf_lat_df <- raster::as.data.frame(ncdf_lat, xy = TRUE, na.rm = TRUE) %>%
        dplyr::rename(lat = X1)
      ncdf_lon_df <- raster::as.data.frame(ncdf_lon, xy = TRUE, na.rm = TRUE) %>%
        dplyr::rename(lon = X1)

      # Convert to a table with original ids (x,y)
      ncdf_dim <- ncdf_lat_df %>%
        dplyr::select(x, y)

      # Get layer names
      name_brick <- names(ncdf_brick)

      ncdf_brick_df <- cbind(
        raster::as.data.frame(raster::extract(x = ncdf_brick, y = ncdf_dim, sp = T)),
        ncdf_dim) %>%
        tibble::as_tibble() %>%
        dplyr::left_join(ncdf_lat_df, by = c("x", "y")) %>%
        dplyr::left_join(ncdf_lon_df, by = c("x", "y")) %>%
        dplyr::select(-x, -y) %>%
        dplyr::mutate(across(c(lat, lon), ~round(., 5)))

      ncdf_grid <- ncdf_brick_df %>%
        dplyr::rename(setNames(c(name_brick, 'lat', 'lon'), c(ncdf_times, 'lat', 'lon'))) %>%
        dplyr::left_join(helios::mapping_wrf_us49, by = c('lat', 'lon')) %>%
        tidyr::gather(key = 'datetime', value = 'value', -lat, -lon, -ID, -subRegion, -region) %>%
        dplyr::mutate(datetime = as.POSIXct(datetime,format = "%Y-%m-%d_%H:%M:%S", tz = "UTC")) %>%
        dplyr::mutate(year = as.character(lubridate::year(datetime)))

      #......................
      # Step 2: Population weighted grid
      #......................

      if(!is.null(population)) {
        # Create population weighted raster if any population years in ncdf years
        if(any(names(population_j_raw) %in% as.character(years))){

          # Replace with helios::mapping_wrf_us49
          population_j_ncdf_grid <- helios::mapping_wrf_us49 %>%
            dplyr::select(ID, subRegion, lat, lon) %>%
            dplyr::left_join(population_j_raw %>%
                               dplyr::mutate(across(c(lat, lon), ~round(., 5))),
                             by = c("lat", "lon")) %>%
            tidyr::gather(key = "year", value = "value", -RID, -lat, -lon, -ID, -subRegion) %>%
            tibble::as_tibble()

          # Weighted population tibble
          print("Starting population weighting ...")
          population_j_weighted <- population_j_ncdf_grid %>%
            # dplyr::left_join(df_polygrid_population_polygon, by=c("x","y")) %>%
            dplyr::group_by(ID, subRegion, year) %>%
            dplyr::mutate(subRegion_total_value = sum(value,na.rm=T)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(pop_weight = value/subRegion_total_value); population_j_weighted
          print("Completed population weighting.")

          #......................
          # Step 4: Calculate Heating and Cooling Degrees (Kelvin to F)
          # Step 5: Population weight for each grid for each year
          #......................
          ncdf_hdcd_pop_weighted <- ncdf_grid %>%
            dplyr::left_join(population_j_weighted %>% dplyr::select(-value, -subRegion_total_value),
                             by = c('ID', 'subRegion', 'lat', 'lon', 'year')) %>%
            dplyr::mutate(value = (((value - 273.15) * 9/5) + 32) - reference_temp_F,
                          value = dplyr::if_else(is.na(pop_weight), value, value*pop_weight))

        }else{
          print(paste0("Population data provided does not contain data for any of the years in the ncdf data."))
          print(paste0("Population data years: ", paste(names(population_j_raw)[!grepl("RID|lat|lon",names(population_j_raw))],collapse=",")))
          print(paste0("ncdf data years: ",as.character(years)))
        }
      }

      #......................
      # Step 3: Subset for time periods chosen
      #......................

      indices <- as.integer(grepl(paste0(time_periods, collapse = "|"), ncdf_times))
      ncdf_times_subset <- ncdf_times[grepl(paste0(time_periods, collapse = "|"), ncdf_times)]
      index_subset <- c(1:length(ncdf_times))*indices
      index_subset <- index_subset[!index_subset %in% 0]

      # Subset raster brick to selected times
      if(length(index_subset) > 0){

        # Equivalent to step 6: Aggregate to regions
        hdcd_region <- ncdf_hdcd_pop_weighted %>%
          dplyr::filter(!is.na(subRegion)) %>%
          dplyr::select(-lat, -lon, -RID) %>%
          dplyr::group_by(subRegion, ID, year, datetime) %>%
          dplyr::summarise(value = dplyr::if_else(any(is.na(pop_weight)), mean(value), sum(value))) %>%
          dplyr::ungroup()

        # Assign HDDCDD categories
        hdcd_region <- hdcd_region %>%
          dplyr::mutate(HDDCDD = dplyr::if_else(value < 0, "HDD", "CDD"))%>%
          dplyr::filter(value!=0)

        #......................
        # Step 7: Aggregate over Segments
        #......................

        temporal_subset <- data.frame(
          ncdf_times = ncdf_times[index_subset],
          x = paste0("X",index_subset)) %>%
          dplyr::mutate(datetime = as.POSIXct(ncdf_times,format = "%Y-%m-%d_%H:%M:%S", tz = "UTC")
                        #datetime =  lubridate::with_tz(datetime,tz="EST")
                        ) %>%
          dplyr::mutate(year = lubridate::year(datetime),
                        month = lubridate::month(datetime),
                        day = lubridate::day(datetime),
                        hour = lubridate::hour(datetime),
                        timezone = lubridate::tz(datetime)) %>%
          dplyr::mutate(year = as.character(year),
                        month = dplyr::if_else(month<10,paste0("0",month),paste0(month)),
                        day = dplyr::if_else(day<10,paste0("0",day),paste0(day)),
                        hour = dplyr::if_else(hour<10,paste0("0",hour),paste0(hour)))

        # hdcd segments
        hdcd_region_segments <- hdcd_region %>%
          dplyr::left_join(temporal_subset, by=c("datetime", "year")) %>%
          dplyr::left_join(helios::segment_map_utc, by=c("subRegion", "month", "day", "hour")) %>%
          dplyr::group_by(subRegion, year, segment, HDDCDD) %>%
          dplyr::summarise(value = sum(value, na.rm=T))

        # hdcd monthly
        hdcd_region_monthly <- hdcd_region %>%
          dplyr::left_join(temporal_subset, by=c("datetime", "year")) %>%
          dplyr::group_by(subRegion, year, month, day) %>%
          dplyr::summarise(value = (max(value) + min(value))/2) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(subRegion, year, month) %>%
          dplyr::summarise(HDD = sum(value[value < 0]),
                           CDD = sum(value[value > 0])) %>%
          dplyr::ungroup() %>%
          tidyr::gather(key = 'HDDCDD', value = 'value', HDD, CDD)


        #......................
        # Step 8: Add in building components for GCAMUSA
        #......................

        hdcd_region_segments_bld <- hdcd_region_segments %>%
          dplyr::left_join(helios::L2441.HDDCDD_Fixed_gcamusa_seg, by = c("subRegion","segment")) %>%
          # Remove columns with -ve hdcd/cooling and +ve/heating
          dplyr::filter(!((value < 0) & grepl("cool",thermal.building.service.input, ignore.case=T)),
                        !((value > 0) & grepl("heat",thermal.building.service.input, ignore.case=T))) %>%
          dplyr::select(-HDDCDD)

        print(paste0("Processing hdcd completed for file: ", ncdf_i))


      } else {
        print(paste0("None of the selected time_periods: ", paste0(time_periods, collapse=", ")))
        print(paste0("are available in the selected ncdf file chosen: ", ncdf_i))
      }

    }} else { # Close if(file.exists(ncdf_i)){
      print(paste0("Skipping hdcd for file which does not exist: ", ncdf_i))
    }

    #......................
    # Step 9a: Save segment data as combined csv files in Level 2 XML format for US or GCAM regions
    # L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv, L2441.HDDCDD_Fixed_rcp8p5_gcamusa.csv,
    # L2441.HDDCDD_Fixed_gcamusa.csv
    #......................

    if(nrow(hdcd_region_segments_bld)>0){
      hdcd_comb <- hdcd_comb %>%
        dplyr::bind_rows(hdcd_region_segments_bld) %>%
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
        filename_i <- paste0(folder, "/hdcd_wrf_to_gcam_intermediate", name_append, ".csv")
        } else {
          filename_i <- paste0(folder, "/hdcd_wrf_to_gcam_", year_min_i, "_", year_max_i, name_append, ".csv")
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
        filename_i_monthly <- paste0(folder,"/hdcd_wrf_to_gcam_intermediate_monthly",name_append,".csv")
      } else {
        filename_i_monthly <- paste0(folder,"/hdcd_wrf_to_gcam_",year_min_i,"_",year_max_i,"_monthly",name_append,".csv")}

      if(save){
      data.table::fwrite(hdcd_comb_monthly, file=filename_i_monthly)
      print(paste0("File saved as : ", filename_i_monthly))
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
  invisible(list(hdcd_comb = hdcd_comb,hdcd_comb_monthly = hdcd_comb_monthly))

} # Close process_hdcd

