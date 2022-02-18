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
      HDDCDD -> noaa_hddcdd -> scenario -> scenario_hddcdd -> ID -> V3 ->
      building.node.input->gcam.consumer->heatcool->month->nodeInput->
      segment->subRegion->thermal.building.service.input->value->x->y->year

  if(is.null(folder)){folder <- paste0(getwd(),"/output")}
  if(save | diagnostics){if(!dir.exists(folder)){dir.create(folder)}}
  # Pick up on intermediate files if program crashed before.
  hdcd_comb <- tibble::tibble()
  hdcd_comb_monthly <- tibble::tibble()
  hdcd_region_segments_bld <- tibble::tibble()
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
      raster::extent(ncdf_brick) <- raster::extent(ncdf_ras) # Fix the resolution and extent of the brick
      # Shape with polygons to map to raster
      if(spatial=="gcamusa"){ shape <- helios::mapUS52}
      # Assign IDs to Polygon Names
      nam <- unique(shape$subRegion)
      nam_df <- data.frame(ID = 1:length(nam), nam = nam)
      shape$ID <- nam_df$ID[match(shape$subRegion,nam_df$nam)]
      # Define raster extent
      raster::extent(ncdf_ras) <- raster::extent(shape)
      # Get Raster ID by Polygon Shape
      ras <- raster::rasterize(x = shape, y = ncdf_ras, field = "ID")
      ras_points <- cbind(raster::xyFromCell(ras, 1:raster::ncell(ras)), raster::values(ras)) %>%
        as.data.frame() %>%
        tibble::as_tibble() %>%
        dplyr::rename(ID=V3)
      df_polygrid <- ras_points  %>%
        dplyr::left_join(shape@data %>% dplyr::select(subRegion,ID), by="ID")

      #......................
      # Step 2: Population weighted grid
      #......................

      if(!is.null(population)) {
      # Create population weighted raster if any population years in ncdf years
      if(any(names(population_j_raw) %in% as.character(years))){
      # Rasterize the population grid to the underlying data grid
      lat_index <- grep("lat", colnames(population_j_raw), ignore.case = T)
      lon_index <- grep("lon", colnames(population_j_raw), ignore.case = T)
      population_j_raw_matrix <- as.matrix(population_j_raw)
      colnames(population_j_raw_matrix) <- names(population_j_raw)
      # you need to provide a function 'fun' for when there are multiple points per cell
      print("rasterizing population data ...")
      population_raster <- raster::rasterize(population_j_raw_matrix[,lon_index:lat_index],
                                     ncdf_ras,
                                     population_j_raw_matrix[,-c(lon_index:lat_index)],
                                     fun=sum)
      print("Completed rasterizing population data.")

      # Get Raster ID by Polygon Shape
      ras_population_polygon <- raster::rasterize(x = shape, y = population_raster, field = "ID")
      ras_points_population_polygon <- cbind(raster::xyFromCell(ras_population_polygon,
                                                                1:raster::ncell(ras_population_polygon)),
                                             raster::values(ras_population_polygon)) %>%
        as.data.frame() %>%
        tibble::as_tibble() %>%
        dplyr::rename(ID=V3)

      df_polygrid_population_polygon <- ras_points_population_polygon  %>%
        dplyr::left_join(shape@data %>% dplyr::select(subRegion,ID), by="ID")


      # Population raster on climate grid as tibble
      population_j_ncdf_grid <- population_raster %>%
        raster::as.data.frame() %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(tibble::as_tibble(raster::xyFromCell(population_raster,
                                                              1:raster::ncell(population_raster)))) %>%
        tidyr::gather(key="year",value="value",-RID,-x,-y) %>%
        tibble::as_tibble(); population_j_ncdf_grid

      # Weighted population tibble
      print("Starting population weighting ...")
      population_j_weighted <- population_j_ncdf_grid %>%
        dplyr::left_join(df_polygrid_population_polygon, by=c("x","y")) %>%
        dplyr::group_by(ID,subRegion,year) %>%
        dplyr::mutate(subRegion_total_value = sum(value,na.rm=T)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(pop_weight = value/subRegion_total_value); population_j_weighted
      print("Completed population weighting.")

      # Weighted population raster

      # Transform to wide
      population_j_weighted_wide <- population_j_weighted %>%
        dplyr::select(-subRegion_total_value, -value, -subRegion, -RID, -ID) %>%
        dplyr::mutate(pop_weight = dplyr::if_else(is.na(pop_weight),0,pop_weight),
                      year = gsub("X","",year,ignore.case = T)) %>%
        tidyr::spread(key="year",value="pop_weight");population_j_weighted_wide

      # Rasterize the population grid to the underlying data grid
      lat_index_weighted <- grep("y", colnames(population_j_weighted_wide), ignore.case = T)
      lon_index_weighted <- grep("x", colnames(population_j_weighted_wide), ignore.case = T)
      population_j_weighted_wide_matrix <- as.matrix(population_j_weighted_wide)
      colnames(population_j_weighted_wide_matrix) <- names(population_j_weighted_wide)
      # you need to provide a function 'fun' for when there are multiple points per cell
      print("rasterizing weighted population data ...")
      population_weighted_raster <- raster::rasterize(population_j_weighted_wide_matrix[,lon_index_weighted:lat_index_weighted],
                                             ncdf_ras,
                                             population_j_weighted_wide_matrix[,-c(lon_index_weighted:lat_index_weighted)],
                                             fun=mean)
      print("Completed rasterizing weighted population data.")
      }else{
        print(paste0("Population data provided does not contain data for any of the years in the ncdf data."))
        print(paste0("Population data years: ", paste(names(population_j_raw)[!grepl("RID|lat|lon",names(population_j_raw))],collapse=",")))
        print(paste0("ncdf data years: ",as.character(years)))
      }
      }

      #......................
      # Step 3: Subset for time periods chosen
      #......................
      indices <- as.integer(grepl(paste0(time_periods,collapse="|"),ncdf_times))
      ncdf_times_subset <- ncdf_times[grepl(paste0(time_periods,collapse="|"),ncdf_times)]
      index_subset <- c(1:length(ncdf_times))*indices
      index_subset <- index_subset[!index_subset %in% 0]

      # Subset raster brick to selected times
      if(length(index_subset)>0){

        ncdf_brick_subset <- ncdf_brick[[index_subset]]

        #......................
        # Step 4: Calculate Heating and Cooling Degrees (Kelvin to F)
        #......................
        ncdf_brick_hdcd <- (((ncdf_brick_subset - 273.15) * 9/5) + 32) - reference_temp_F

        #......................
        # Step 5: Population weight for each year by multiplying with weights
        #......................
        # Population weight if population grid provided

        pop_weighted = 0

        if(!is.null(population)) {
        if(any(names(population_j_raw) %in% as.character(years))){

          ncdf_brick_hdcd_pop <- raster::brick()

          # If population data for year exists mulitply by population weight for that year
          years = unique(substr(ncdf_times,1,4))
          for(year_i in years){

            # Subset ncdf_brick_hdcd
            indices_pop <- as.integer(grepl(paste0(year_i,collapse="|"),ncdf_times_subset))
            index_subset_pop <- c(1:length(ncdf_times))*indices_pop
            index_subset_pop <- index_subset_pop[!index_subset_pop %in% 0]
            ncdf_brick_hdcd_pop_i <- ncdf_brick_hdcd[[index_subset_pop]]

            # Subset population to the year
            indices_pop_w <- as.integer(grepl(paste0(year_i,collapse="|"),names(population_weighted_raster)))
            index_subset_pop_w <- c(1:length(names(population_weighted_raster)))*indices_pop_w
            index_subset_pop_w <- index_subset_pop_w[!index_subset_pop_w %in% 0]

            if(length(index_subset_pop_w)>0){

            population_weighted_raster_i <- population_weighted_raster[[index_subset_pop_w]]
            # Create a brick for population data with same dimensions as ncdf_brick_hdcd
            population_weighted_raster_i_brick <- raster::brick(replicate(length(names(ncdf_brick_hdcd_pop_i)),population_weighted_raster_i))

            # Check dimensions
            if(all(dim(ncdf_brick_hdcd_pop_i)==dim(population_weighted_raster_i_brick))){
              # Multiple ncdf_brick_hdcd with population year
              raster::extent(population_weighted_raster_i_brick) <- raster::extent(ncdf_brick_hdcd_pop_i) # Fix the resolution and extent of the brick
              ncdf_brick_hdcd_pop_i_weighted <- ncdf_brick_hdcd_pop_i * population_weighted_raster_i_brick
              names(ncdf_brick_hdcd_pop_i_weighted) <- names(ncdf_brick_hdcd_pop_i)

              # Append to brick
              ncdf_brick_hdcd_pop <- raster::brick(raster::stack(ncdf_brick_hdcd_pop,ncdf_brick_hdcd_pop_i_weighted))
              pop_weighted = 1

            } else {
              print(paste0("Dimensions of population and hdcd rasters do not match. Skipping population weighting."))
            }

            } else { # Close if(length(index_subset_pop_w)>0){

              print(paste0("Year: ", year_i," in hdcd raster does not exist in population raster. Skipping population weighting."))

            }

          }

        } else {
          ncdf_brick_hdcd_pop <- ncdf_brick_hdcd
        }} else {
          ncdf_brick_hdcd_pop <- ncdf_brick_hdcd
        }

        #......................
        # Step 6: Aggregate to regions
        #......................
        # Combine with ncdf_grid and aggregate to regions
        # If population weighted then sum the wieghted grids
        if(pop_weighted == 1){
          hdcd_region <- df_polygrid %>%
            dplyr::bind_cols(
              ncdf_brick_hdcd_pop %>%
                raster::as.data.frame() %>%
                tibble::as_tibble()) %>%
            dplyr::select(-x,-y) %>%
            dplyr::group_by(subRegion, ID) %>%
            dplyr::summarise_all(list(~sum(.,na.rm=T))) %>%
            tidyr::gather(key="x",value="value", -ID,-subRegion)}

        # If not population weighted then take the mean of the grids for the total region
        if(pop_weighted == 0){
          hdcd_region <- df_polygrid %>%
            dplyr::bind_cols(
              ncdf_brick_hdcd_pop %>%
                raster::as.data.frame() %>%
                tibble::as_tibble()) %>%
            dplyr::select(-x,-y) %>%
            dplyr::group_by(subRegion, ID) %>%
            dplyr::summarise_all(list(~mean(.,na.rm=T))) %>%
            tidyr::gather(key="x",value="value", -ID,-subRegion)}

        #......................
        # Step 7: Aggregate over Segments
        #......................

        temporal_subset <- data.frame(
          ncdf_times = ncdf_times[index_subset],
          x = paste0("X",index_subset))

        # hdcd segments
        hdcd_region_segments <- hdcd_region %>%
          dplyr::left_join(temporal_subset, by="x") %>%
          dplyr::mutate(year = substr(ncdf_times,1,4),
                        month = substr(ncdf_times,6,7),
                        day = substr(ncdf_times,9,10),
                        hour = substr(ncdf_times,12,13)) %>%
          dplyr::left_join(helios::segment_map_utc, by=c("subRegion", "month", "day", "hour")) %>%
          dplyr::select(subRegion,segment,year,value) %>%
          dplyr::group_by(subRegion,year,segment) %>%
          dplyr::summarize(value=sum(value,na.rm=T)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(subRegion))

        # hdcd monthly
        if(diagnostics){
        hdcd_region_monthly <- hdcd_region %>%
          dplyr::left_join(temporal_subset, by="x") %>%
          dplyr::mutate(year = substr(ncdf_times,1,4),
                        month = substr(ncdf_times,6,7),
                        day = substr(ncdf_times,9,10),
                        hour = substr(ncdf_times,12,13)) %>%
          dplyr::select(subRegion,year,month,value) %>%
          dplyr::group_by(subRegion,year, month) %>%
          dplyr::summarize(value=sum(value,na.rm=T)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(subRegion))
        }

        #......................
        # Step 8: Add in building components for GCAMUSA
        #......................

        hdcd_region_segments_bld <- hdcd_region_segments %>%
          dplyr::left_join(helios::L2441.HDDCDD_Fixed_gcamusa_seg, by = c("subRegion","segment")) %>%
          # Remove columns with -ve hdcd/cooling and +ve/heating
          dplyr::filter(!((value < 0) & grepl("cool",thermal.building.service.input, ignore.case=T)),
                        !((value > 0) & grepl("heat",thermal.building.service.input, ignore.case=T)))

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

    if(nrow(hdcd_comb)>0){
      hdcd_comb <- hdcd_comb %>%
        dplyr::bind_rows(hdcd_region_segments_bld) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(subRegion,year,segment,gcam.consumer,
                        nodeInput, building.node.input,
                        thermal.building.service.input) %>%
        dplyr::summarize(value=sum(value,na.rm=T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(subRegion),
                      !is.na(year),
                      !is.na(segment))

      year_min_i <- min(hdcd_comb$year,na.rm=T)
      year_max_i <- max(hdcd_comb$year,na.rm=T)

      if(i < length(ncdf)){
        filename_i <- paste0(folder,"/hdcd_wrf_to_gcam_intermediate",name_append,".csv")
        } else {
          filename_i <- paste0(folder,"/hdcd_wrf_to_gcam_",year_min_i,"_",year_max_i,name_append,".csv")
          }

      if(save){
        data.table::fwrite(hdcd_comb, file=filename_i)
        print(paste0("File saved as : ", filename_i))
        }
      }

    #......................
    # Step 9b: Save monthly as combined csv files
    #......................

    if(diagnostics){
     if(nrow(hdcd_comb_monthly)>0){

    hdcd_comb_monthly <- hdcd_comb_monthly %>%
      dplyr::bind_rows(hdcd_region_monthly) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(subRegion,year,month) %>%
      dplyr::summarize(value=sum(value,na.rm=T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(subRegion),
                    !is.na(year),
                    !is.na(month))

    year_min_i <- min(hdcd_comb_monthly$year,na.rm=T)
    year_max_i <- max(hdcd_comb_monthly$year,na.rm=T)

    if(i < length(ncdf)){
      filename_i_monthly <- paste0(folder,"/hdcd_wrf_to_gcam_intermediate_monthly",name_append,".csv")
    } else {
      filename_i_monthly <- paste0(folder,"/hdcd_wrf_to_gcam_",year_min_i,"_",year_max_i,"_monthly",name_append,".csv")}

    if(save){
    data.table::fwrite(hdcd_comb_monthly, file=filename_i_monthly)
    print(paste0("File saved as : ", filename_i_monthly))
    }

    }}

  } # Close for(i in 1:length(ncdf)){


  #......................
  # Step 10: Save XML
  #......................

  if(xml){
  # Format to match GCAM output file L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv
  if(nrow(hdcd_comb)>0){
    hdcd_comb_xml <- hdcd_comb %>%
    dplyr::select(region=subRegion,
                  gcam.consumer,
                  nodeInput,
                  building.node.input,
                  thermal.building.service.input,
                  year,
                  degree.days=value)

  filename_i_xml <- gsub(".csv",".xml",filename_i)

  gcamdata::create_xml(filename_i_xml) %>%
    gcamdata::add_xml_data(hdcd_comb_xml, "HDDCDD")%>%
    gcamdata::run_xml_conversion()

  print(paste0("File saved as : ", filename_i_xml))
  }
    }


  #......................
  # Step 11: Diagnostics
  #......................

  if(diagnostics){

    print(".........................................")
    print("Starting doagnostics ...")

    folder_diagnostics <- paste0(folder,"/diagnostics")
    if(!dir.exists(folder_diagnostics)){dir.create(folder_diagnostics)}

    if(nrow(hdcd_comb)>0){

    #..............
    # By Segment
    #.............

    hdcd_comb_diagnostics <- hdcd_comb %>%
      dplyr::select(subRegion,year,segment,value) %>%
      unique() %>%
      dplyr::mutate(heatcool = dplyr::if_else(value < 0, "heat","cool"))

    segment_levels = c("Jan_day","Jan_night","Feb_day","Feb_night",
                       "Mar_day","Mar_night","Apr_day","Apr_night",
                       "May_day","May_night","Jun_day","Jun_night",
                       "Jul_day","Jul_night","Aug_day","Aug_night",
                       "Sep_day","Sep_night","Oct_day","Oct_night",
                       "Nov_day","Nov_night","Dec_day","Dec_night","superpeak")

    # Individul Years
    for(year_i in (hdcd_comb_diagnostics$year) %>% unique()) {
      ggplot2::ggplot(data = hdcd_comb_diagnostics %>%
                        dplyr::filter(year == year_i) %>%
                        dplyr::mutate(segment = factor(segment, levels = segment_levels))) +
        ggplot2::aes(x = segment, y = value, group = heatcool) +
        ggplot2::geom_line(ggplot2::aes(color = heatcool)) +
        ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste0("HDCD WRF to GCAM ", year_i , " ")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =
                                                             0.5))+
        ggplot2::scale_color_manual(values = c("heat" = "firebrick", "cool" =
                                                 "dodgerblue")) +
        ggplot2::scale_x_discrete(drop=FALSE)

        filename_diagnostics_i <-
          paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename_i)), "_", year_i,name_append,".png")

        ggplot2::ggsave(filename =  filename_diagnostics_i,
                        width = 25,
                        height = 15) # save plot

        print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    # Combined years free scale
    if(T) {
      ggplot2::ggplot(data = hdcd_comb_diagnostics %>%
                        dplyr::mutate(segment = factor(segment, levels = segment_levels))) +
        ggplot2::aes(x = segment, y = value, group = year) +
        ggplot2::geom_line(ggplot2::aes(color = heatcool)) +
        ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste0("HDCD WRF to GCAM ")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =
                                                             0.5))+
        ggplot2::scale_color_manual(values = c("heat" = "firebrick", "cool" =
                                                 "dodgerblue"))+
      ggplot2::scale_x_discrete(drop=FALSE)

      filename_diagnostics_i <-
        paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename_i)), "_allYears_freeScale",name_append,".png")

      ggplot2::ggsave(filename =  filename_diagnostics_i,
                      width = 25,
                      height = 15) # save plot

      print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    # Combined years fixed scale
    if(T) {
      ggplot2::ggplot(data = hdcd_comb_diagnostics %>%
                        dplyr::mutate(segment = factor(segment, levels = segment_levels))) +
        ggplot2::aes(x = segment, y = value, group = year) +
        ggplot2::geom_line(ggplot2::aes(color = heatcool)) +
        ggplot2::facet_wrap(subRegion ~ ., scales = "fixed") +
        ggplot2::ggtitle(paste0("HDCD WRF to GCAM ")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =
                                                             0.5))+
        ggplot2::scale_color_manual(values = c("heat" = "firebrick", "cool" =
                                                 "dodgerblue"))+
        ggplot2::scale_x_discrete(drop=FALSE)

      filename_diagnostics_i <-
        paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename_i)), "_allYears_fixedScale",name_append,".png")

      ggplot2::ggsave(filename =  filename_diagnostics_i,
                      width = 25,
                      height = 15) # save plot

      print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    }

    #..............
    # By Month compare against NOAA
    #.............

    if(nrow(hdcd_comb_monthly)>0){
    months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthNums = c("01","02","03","04","05","06","07","08","09","10","11","12")
    monthsShort<- c("JA","FB","MR","AP","MY","JN","JL","AG","SP","OC","NV","DC")
    months_df <- data.frame(month=months,
                            monthNums = monthNums,
                            monthsShort = monthsShort)

    hdcd_comb_monthly_diagnostics <- hdcd_comb_monthly %>%
      dplyr::select(subRegion,year,monthNums = month,value) %>%
      unique() %>%
      dplyr::mutate(HDDCDD = dplyr::if_else(value < 0, "HDD","CDD")) %>%
      dplyr::mutate(value = abs(value),
                    scenario = "ncdf") %>%
      dplyr::left_join(months_df, by=c("monthNums")) %>%
      dplyr::bind_rows(helios::noaa_hddcdd %>%
                         dplyr::select(subRegion=stateCode, year, month, HDDCDD, value) %>%
                         dplyr::mutate(scenario ="noaa",
                                       year = as.character(year)) %>%
                         dplyr::left_join(months_df, by=c("month"))) %>%
      dplyr::mutate(month = factor(month, levels = months))

    # Find closest matching years
    current_years <- as.integer(unique(hdcd_comb_monthly$year))
    noaa_years <- as.integer(unique(noaa_hddcdd$year))

    # Individul Years
    for(year_i in current_years) {

      noaa_year_i <- noaa_years[which(abs(noaa_years - year_i) == min(abs(noaa_years - year_i)))]

      hdcd_comb_monthly_diagnostics %>%
        dplyr::filter((year == year_i & scenario == "ncdf") |
                        (year == noaa_year_i & scenario == "noaa")) %>%
        dplyr::mutate(scenario = paste0(scenario,"_",year)) %>%
        dplyr::select(subRegion, scenario, year, month,HDDCDD, value)->
        hdcd_comb_monthly_diagnostics_i

      # Expand to include all year months
      all <- hdcd_comb_monthly_diagnostics_i %>%
        tidyr::expand(subRegion,scenario,year,month,HDDCDD)
      hdcd_comb_monthly_diagnostics_i %>%
        dplyr::right_join(all) %>%
        dplyr::filter((year == year_i & scenario == paste0("ncdf_",year_i)) |
                        (year == noaa_year_i & scenario == paste0("noaa_",noaa_year_i))) %>%
        dplyr::mutate(scenario_hddcdd = paste0(scenario,HDDCDD)) %>%
        tidyr::replace_na(list(value=0))->
        hdcd_comb_monthly_diagnostics_i

      ggplot2::ggplot(data = hdcd_comb_monthly_diagnostics_i,
                      ggplot2::aes(x = month, y = value, group=scenario_hddcdd)) +
        ggplot2::geom_line(ggplot2::aes(color = HDDCDD, linetype = scenario)) +
        ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste0("NCDF_", year_i," NOAA_",noaa_year_i)) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust =
                                                             0.5))+
        ggplot2::scale_color_manual(values = c("HDD" = "firebrick",
                                               "CDD" = "dodgerblue")) +
        ggplot2::scale_linetype_manual(values = c(1,2)) +
        ggplot2::scale_x_discrete(drop=FALSE)

      filename_monthly_diagnostics_i <-
        paste0(folder_diagnostics, "/monthly_ncdf_", year_i,"_noaa_",noaa_year_i,name_append,".png")

      ggplot2::ggsave(filename =  filename_monthly_diagnostics_i,
                      width = 25,
                      height = 15) # save plot

      print(paste0("Diagnostic figure saved as ", filename_monthly_diagnostics_i))
    }

    }

    #...............

    print("Diagnostics complete.")

  }


  #......................
  # Close out
  #......................

  print("process_hdcd completed.")

  # return data
  invisible(list(hdcd_comb = hdcd_comb,hdcd_comb_monthly = hdcd_comb_monthly))

} # Close process_hdcd

