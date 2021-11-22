#' hdcd
#'
#' Heating and Cooling Degree processing for GCAM from various sources such as WRF and CMIP
#'
#' @param ncdf Default = NULL. Path to ncdf file.
#' @param spatial Default = NULL.  "gcamusa"
#' @param temporal Default = NULL
#' @param population Default = NULL
#' @param reference_temp_F Default = 65
#' @param folder Default = paste0(getwd(),"/output").
#' @param diagnostics Default = F.
#' @importFrom magrittr %>%
#' @export

hdcd <- function(ncdf = NULL,
                 spatial = NULL,
                 temporal = NULL,
                 population = NULL,
                 reference_temp_F = 65,
                 folder = paste0(getwd(),"/output"),
                 diagnostics = T) {

  #............
  # For Testing
  #............
  # ncdf = "wrfout_d01_1979-01-01_00%3A00%3A00"
  # spatial = "gcamusa"
  # temporal = "gcamusa"
  # population = NULL
  # reference_temp_F = 65

  print("Starting function process_hdcd...")

  #......................
  # Initialize
  #......................

  NULL -> ID -> V3 -> building.node.input -> day -> day_night -> gcam.consumer ->
    hour -> is_super_peak -> month -> nodeInput -> read.csv -> region -> segment ->
    state ->  subRegion -> thermal.building.service.input -> value -> values ->
    x -> y -> year -> filename_diagnostics_i -> heatcool

  if(!dir.exists(folder)){dir.create(folder)}
  if(is.null(folder)){folder <- paste0(getwd(),"/output")}
  # Pick up on intermediate files if program crashed before.
  hdcd_comb <- tibble::tibble()

  #......................
  # Loop over each ncdf file
  #......................

  print(paste0("Processing files provided: ",paste0(ncdf,collapse=", ")))

  for(i in 1:length(ncdf)){

    ncdf_i <- ncdf[i]

    if(file.exists(ncdf_i)){

      print(".........................................")
      print(paste0("Running hdcd for file: ", ncdf_i))

      #......................
      # Check Inputs
      #......................

      ncdf_in <- ncdf4::nc_open(ncdf_i)
      ncdf_brick <- raster::brick(ncdf_i,varname="T2",ncdf=TRUE)

      #......................
      # Step 1: Map grid (lat/lon) to each shape in the polygons being mapped to
      #......................
      # Base raster
      ncdf_ras <- ncdf_brick[[1]]
      # Shape with polygons to map to raster
      if(spatial=="gcamusa"){ shape <- helios::mapUS52}
      # Assign IDs to Polygon Names
      nam <- unique(shape$subRegion)
      nam_df <- data.frame(ID = 1:length(nam), nam = nam)
      shape$ID <- nam_df$ID[match(shape$subRegion,nam_df$nam)]
      # Define raster extent
      raster::extent(ncdf_ras) <- raster::extent(shape)
      # Rasterize
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
      # population in cell/population in region
      # Apply population weight to each grid

      #......................
      # Step 3: Subset for time periods chosen
      #......................
      # Index of Times available
      ncdf_times <-  ncdf4::ncvar_get(ncdf_in,"Times")
      # Assign time_periods
      if(is.null(temporal)){time_periods = ncdf_times} else {
        if(temporal=="gcamusa"){time_periods = seq(2020,2100,by=5)}
      }
      indices <- as.integer(grepl(paste0(time_periods,collapse="|"),ncdf_times))
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
        # Population wieght if population grid provided
        if(!is.null(population)){

        } else {
          ncdf_brick_hdcd_pop <- ncdf_brick_hdcd
        }

        #......................
        # Step 6: Aggregate to regions
        #......................
        # Combine with ncdf_grid and aggregate to regions
        hdcd_region <- df_polygrid %>%
          dplyr::bind_cols(
            ncdf_brick_hdcd_pop %>%
              raster::as.data.frame() %>%
              tibble::as_tibble()) %>%
          dplyr::select(-x,-y) %>%
          dplyr::group_by(subRegion, ID) %>%
          dplyr::summarise_all(list(~mean(.,na.rm=T))) %>%
          tidyr::gather(key="x",value="value", -ID,-subRegion)

        #......................
        # Step 7: Aggregate over Segments
        #......................

        temporal_subset <- data.frame(
          ncdf_times = ncdf_times[index_subset],
          x = paste0("X",index_subset))

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

    } else { # Close if(file.exists(ncdf_i)){
      print(paste0("Skipping hdcd for file which does not exist: ", ncdf_i))
    }

    #......................
    # Step 9: Save as combined csv files in Level 2 XML format for US or GCAM regions
    #......................

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
      filename_i <- paste0(folder,"/hdcd_wrf_to_gcam_intermediate.csv")
      } else {
        filename_i <- paste0(folder,"/hdcd_wrf_to_gcam_",year_min_i,"_",year_max_i,".csv")}

    data.table::fwrite(hdcd_comb, file=filename_i)
    print(paste0("File saved as : ", filename_i))

  } # Close for(i in 1:length(ncdf)){

  #......................
  # Step 10: Diagnostics
  #......................

  if(diagnostics){

    print(".........................................")
    print("Starting doagnostics ...")

    folder_diagnostics <- paste0(folder,"/diagnostics")
    if(!dir.exists(folder_diagnostics)){dir.create(folder_diagnostics)}

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
                                                 "dodgerblue"))
        #ggplot2::scale_x_discrete(drop=FALSE)

        filename_diagnostics_i <-
          paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename_i)), "_", year_i, ".png")

        ggplot2::ggsave(filename =  filename_diagnostics_i,
                        width = 13,
                        height = 10) # save plot

        print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    print("Doagnostics complete.")

  }


  #......................
  # Close out
  #......................

  print("process_hdcd completed.")

} # Close process_hdcd

