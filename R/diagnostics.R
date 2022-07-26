#' diagnostics
#'
#' Heating and Cooling Degree diagnostic scripts
#'
#' @param hdcd Default = tibble::tibble()
#' @param hdcd_monthly Default = tibble::tibble()
#' @param folder Default = paste0(getwd()).
#' @param filename Default = "hdcd_diagnostic".
#' @param name_append Default = "". Name to append to all filenames
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

diagnostics <- function(hdcd = tibble::tibble(),
                        hdcd_monthly = tibble::tibble(),
                        folder = paste0(getwd()),
                        filename = "hdcd_diagnostic",
                        name_append = "") {

  print("Starting function diagnostics...")

  #......................
  # Initialize
  #......................

  if(T){

    NULL -> subRegion -> year -> segment -> value -> heatcool ->
      month -> stateCode -> HDDCDD -> scenario -> scenario_hddcdd

  }

  #......................
  # Diagnostics
  #......................

    folder_diagnostics <- paste0(folder,"/diagnostics",name_append)
    if(!dir.exists(folder_diagnostics)){dir.create(folder_diagnostics)}

    if(nrow(hdcd) > 0){

    #..............
    # By Segment
    #.............

    hdcd_comb_diagnostics <- hdcd %>%
      dplyr::select(subRegion, year, segment, value) %>%
      unique() %>%
      dplyr::mutate(heatcool = dplyr::if_else(value < 0, "heat","cool"),
                    value = abs(value))

    hdcd_comb_year_range <- paste(min(unique(hdcd_comb_diagnostics$year)),
                                  max(unique(hdcd_comb_diagnostics$year)),
                                  sep = '-')

    filename_diagnostics <-
      paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename)), "_",
             hdcd_comb_year_range, name_append, '.csv')

    # save plot data for segment diagnostic
    data.table::fwrite(x = hdcd_comb_diagnostics,
                       file = filename_diagnostics)

    print(paste0("Diagnostic table saved as ", filename_diagnostics))

    segment_levels = c("Jan_day","Jan_night","Feb_day","Feb_night",
                       "Mar_day","Mar_night","Apr_day","Apr_night",
                       "May_day","May_night","Jun_day","Jun_night",
                       "Jul_day","Jul_night","Aug_day","Aug_night",
                       "Sep_day","Sep_night","Oct_day","Oct_night",
                       "Nov_day","Nov_night","Dec_day","Dec_night","superpeak")

    # Individual Years
    for(year_i in (hdcd_comb_diagnostics$year) %>% unique()) {
      data_plot <- hdcd_comb_diagnostics %>%
        dplyr::filter(year == year_i) %>%
        dplyr::mutate(segment = factor(segment, levels = segment_levels))

      ggplot2::ggplot(data = data_plot) +
        ggplot2::aes(x = segment, y = value, group = heatcool) +
        ggplot2::geom_line(ggplot2::aes(color = heatcool)) +
        ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste0("HDCD WRF to GCAM ", year_i , " ")) +
        ggplot2::scale_color_manual(values = c("heat" = "firebrick",
                                               "cool" = "dodgerblue")) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 0.5))

        filename_diagnostics_i <-
          paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename)), "_", year_i,name_append, '.png')

        # save plot
        ggplot2::ggsave(filename = filename_diagnostics_i,
                        width = 25,
                        height = 15)

        print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    # Combined years free scale
    if(T) {
      data_plot <- hdcd_comb_diagnostics %>%
        dplyr::mutate(segment = factor(segment, levels = segment_levels))

      ggplot2::ggplot(data = data_plot) +
        ggplot2::aes(x = segment, y = value,
                     group = interaction(year, heatcool),
                     color = interaction(heatcool)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste0("HDCD WRF to GCAM ")) +
        ggplot2::scale_color_manual(values = c("heat" = "firebrick",
                                               "cool" = "dodgerblue")) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::guides(color = ggplot2::guide_legend(title = 'heatcool')) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 0.5),
                       legend.position = 'right')

      filename_diagnostics_i <-
        paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename)), "_allYears_freeScale", name_append, '.png')

      # save plot
      ggplot2::ggsave(filename = filename_diagnostics_i,
                      width = 25,
                      height = 15)

      print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    # Combined years fixed scale
    if(T) {
      data_plot <- hdcd_comb_diagnostics %>%
        dplyr::mutate(segment = factor(segment, levels = segment_levels))

      ggplot2::ggplot(data = data_plot) +
        ggplot2::aes(x = segment, y = value,
                     group = interaction(year, heatcool),
                     color = interaction(heatcool)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(subRegion ~ ., scales = "fixed") +
        ggplot2::ggtitle(paste0("HDCD WRF to GCAM ")) +
        ggplot2::scale_color_manual(values = c("heat" = "firebrick", "cool" =
                                                 "dodgerblue")) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::guides(color = ggplot2::guide_legend(title = 'heatcool')) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 0.5))

      filename_diagnostics_i <-
        paste0(folder_diagnostics, "/", basename(gsub(".csv", "", filename)), "_allYears_fixedScale",name_append,".png")

      ggplot2::ggsave(filename =  filename_diagnostics_i,
                      width = 25,
                      height = 15) # save plot

      print(paste0("Diagnostic figure saved as ", filename_diagnostics_i))
    }

    }

    #..............
    # By Month compare against NOAA
    #.............

    if(nrow(hdcd_monthly) > 0){
    months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthNums = c("01","02","03","04","05","06","07","08","09","10","11","12")
    monthsShort<- c("JA","FB","MR","AP","MY","JN","JL","AG","SP","OC","NV","DC")
    months_df <- data.frame(month=months,
                            monthNums = monthNums,
                            monthsShort = monthsShort)

    hdcd_comb_monthly_diagnostics <- hdcd_monthly %>%
      dplyr::mutate(month = as.character(month),
                    month = dplyr::if_else(month=="1","01",month),
                    month = dplyr::if_else(month=="2","02",month),
                    month = dplyr::if_else(month=="3","03",month),
                    month = dplyr::if_else(month=="4","04",month),
                    month = dplyr::if_else(month=="5","05",month),
                    month = dplyr::if_else(month=="6","06",month),
                    month = dplyr::if_else(month=="7","07",month),
                    month = dplyr::if_else(month=="8","08",month),
                    month = dplyr::if_else(month=="9","09",month),
                    year = as.character(year)) %>%
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
    current_years <- as.integer(unique(hdcd_monthly$year))
    noaa_years <- as.integer(unique(helios::noaa_hddcdd$year))

    filename_monthly_diagnostics <-
      paste0(folder_diagnostics,
             "/monthly_ncdf_", min(current_years), '-', max(current_years),
             "_noaa_", min(noaa_years), '-', max(noaa_years), name_append,".csv")

    # save plot data for monthly diagnostic
    data.table::fwrite(x = hdcd_comb_monthly_diagnostics,
                       file = filename_monthly_diagnostics)
    print(paste0("Diagnostic table saved as ", filename_monthly_diagnostics))

    # Individual Years
    for(year_i in intersect(current_years, noaa_years)) {

      noaa_year_i <- noaa_years[which(abs(noaa_years - year_i) == min(abs(noaa_years - year_i)))]

      hdcd_comb_monthly_diagnostics %>%
        dplyr::filter((year == year_i & scenario == "ncdf") |
                        (year == noaa_year_i &
                           scenario == "noaa")) %>%
        dplyr::mutate(scenario = paste0(scenario, "_", year)) %>%
        dplyr::select(subRegion, scenario, year, month, HDDCDD, value) ->
        hdcd_comb_monthly_diagnostics_i

      # Expand to include all year months
      all <- hdcd_comb_monthly_diagnostics_i %>%
        tidyr::expand(subRegion, scenario, year, month, HDDCDD)
      hdcd_comb_monthly_diagnostics_i %>%
        dplyr::right_join(all) %>%
        dplyr::filter((year == year_i &
                         scenario == paste0("ncdf_", year_i)) |
                        (year == noaa_year_i &
                           scenario == paste0("noaa_", noaa_year_i))) %>%
        dplyr::mutate(scenario_hddcdd = paste0(scenario, HDDCDD)) %>%
        tidyr::replace_na(list(value = 0)) ->
        hdcd_comb_monthly_diagnostics_i

      ggplot2::ggplot(data = hdcd_comb_monthly_diagnostics_i,
                      ggplot2::aes(x = month, y = value, group = scenario_hddcdd)) +
        ggplot2::geom_line(ggplot2::aes(color = HDDCDD, linetype = scenario)) +
        ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste0("NCDF_", year_i, " NOAA_", noaa_year_i)) +
        ggplot2::scale_color_manual(values = c("HDD" = "firebrick",
                                               "CDD" = "dodgerblue")) +
        ggplot2::scale_linetype_manual(values = c(1, 2)) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 0.5))

      filename_monthly_diagnostics_i <-
        paste0(folder_diagnostics, "/monthly_ncdf_", year_i,"_noaa_",noaa_year_i,name_append,".png")

      ggplot2::ggsave(filename =  filename_monthly_diagnostics_i,
                      width = 25,
                      height = 15) # save plot

      print(paste0("Diagnostic figure saved as ", filename_monthly_diagnostics_i))
    }

    # All Years Together
    noaa_year_latest <- noaa_years[length(noaa_years)]

    noaa_latest <- hdcd_comb_monthly_diagnostics %>%
      dplyr::filter((year == noaa_year_latest & scenario == "noaa") &
                      (!subRegion %in% c('AK', 'HI'))) %>%
      dplyr::rename(noaa = value) %>%
      dplyr::select(subRegion, month, HDDCDD, noaa)

    hdcd_comb_monthly_diagnostics_all <- hdcd_comb_monthly_diagnostics %>%
      dplyr::filter(scenario == "ncdf" &
                      (!subRegion %in% c('AK', 'HI'))) %>%
      dplyr::left_join(noaa_latest, by = c('subRegion', 'month', 'HDDCDD')) %>%
      dplyr::mutate(noaa = dplyr::if_else(is.na(noaa), 0, noaa)) %>%
      dplyr::select(subRegion, scenario, year, month, HDDCDD, value, noaa)

    n_color <- length(unique(hdcd_comb_monthly_diagnostics_all$year))
    pal_hd <- colorRampPalette(RColorBrewer::brewer.pal(9, 'YlOrRd'))
    pal_cd <-  colorRampPalette(RColorBrewer::brewer.pal(9, 'YlGnBu'))
    pal <- c(rev(pal_hd(n_color)), rev(pal_cd(n_color)))

    filename_monthly_diagnostics_all <-
      paste0(folder_diagnostics, "/monthly_ncdf_AllYears", "_noaa_",noaa_year_latest, name_append,".png")

    ggplot2::ggplot(data = hdcd_comb_monthly_diagnostics_all) +
      ggplot2::geom_line(ggplot2::aes(x = month, y = value,
                                      group = interaction(scenario, year, HDDCDD),
                                      color = interaction(year, HDDCDD))) +
      ggplot2::geom_line(ggplot2::aes(x = month, y = noaa, group = HDDCDD),
                         color = 'black', size = 1.2, linetype = 'dashed') +
      ggplot2::facet_wrap(subRegion ~ ., scales = "free_y") +
      ggplot2::ggtitle(paste0("NCDF (all years) VS", " NOAA (", noaa_year_latest, ')')) +
      ggplot2::ylab('Monthly Degree-Hours') +
      ggplot2::scale_color_manual(values = pal,
                                  guide = ggplot2::guide_legend(title = 'NCDF (All Years)')) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5))

    ggplot2::ggsave(filename = filename_monthly_diagnostics_all,
                    width = 25,
                    height = 15)
    print(paste0("Diagnostic figure saved as ", filename_monthly_diagnostics_all))

    }

    #...............

    print("Diagnostics complete.")

  }
