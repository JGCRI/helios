#' diagnostics
#'
#' Heating and Cooling Degree diagnostic scripts
#'
#' @param hdcd_segment Default = tibble::tibble(). hdcd output by segment
#' @param hdcd_monthly Default = tibble::tibble(). hdcd output by month
#' @param min_diagnostic_months Default = 6. Months in the outputs need to exceed this limit to trigger diagnostic plots. Max months is 12.
#' @param folder Default = paste0(getwd()).
#' @param name_append Default = ''. Name to append to all filenames
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom grDevices colorRampPalette
#' @export

diagnostics <- function(hdcd_segment = tibble::tibble(),
                        hdcd_monthly = tibble::tibble(),
                        min_diagnostic_months = 6,
                        folder = paste0(getwd()),
                        name_append = '') {

  print('Starting function diagnostics...')
  #......................
  # Initialize
  #......................

  if(T){

    NULL -> subRegion -> year -> segment -> value -> heatcool ->
      month -> stateCode -> HDCD -> scenario -> scenario_hdcd ->
      noaa -> HDDCDD

  }

  #......................
  # Diagnostics
  #......................

  # create diagnostics folder if not exists
  folder_diagnostics <- file.path(folder, helios::create_name(c('diagnostics', name_append)))
  if (!dir.exists(folder_diagnostics)) {
    dir.create(folder_diagnostics, recursive = T)
  }


  if(all(nrow(hdcd_segment) > 0, 'segment' %in% names(hdcd_segment))){

    # --------------------------------------
    # By Segment
    # --------------------------------------

    # GCAM dispatch segments
    segment_levels <- c('Jan_day', 'Jan_night', 'Feb_day', 'Feb_night',
                        'Mar_day', 'Mar_night', 'Apr_day', 'Apr_night',
                        'May_day', 'May_night', 'Jun_day', 'Jun_night',
                        'Jul_day', 'Jul_night', 'Aug_day', 'Aug_night',
                        'Sep_day', 'Sep_night', 'Oct_day', 'Oct_night',
                        'Nov_day', 'Nov_night', 'Dec_day', 'Dec_night',
                        'superpeak')

    if(any(unique(hdcd_segment$segment) %in% segment_levels)) {

      # Check if the outputs cover a full year
      # Only plot when the outputs are more than 4 months
      if(length(unique(hdcd_segment$segment)) >= min_diagnostic_months * 2) {

        # assign heat and cool based on the value
        hdcd_comb_diagnostics <- hdcd_segment %>%
          dplyr::select(subRegion, year, segment, value) %>%
          unique() %>%
          dplyr::mutate(heatcool = dplyr::if_else(value < 0, 'heat','cool'),
                        value = abs(value))

        # range of data time span
        hdcd_comb_year_range <- paste(unique(c(min(unique(hdcd_comb_diagnostics$year)),
                                               max(unique(hdcd_comb_diagnostics$year)))),
                                      collapse = '-')

        # create table name
        filename_diagnostics <- file.path(
          folder_diagnostics,
          helios::create_name(c('segment', hdcd_comb_year_range, name_append), 'csv'))

        # save plot data for segment diagnostic
        data.table::fwrite(x = hdcd_comb_diagnostics,
                           file = filename_diagnostics)

        print(paste0('Diagnostic table saved as : ', filename_diagnostics))

        # Individual Years
        for(year_i in (hdcd_comb_diagnostics$year) %>% unique()) {

          data_plot <- hdcd_comb_diagnostics %>%
            dplyr::filter(year == year_i) %>%
            dplyr::mutate(segment = factor(segment, levels = segment_levels))

          # plot
          p <- ggplot2::ggplot(data = data_plot) +
            ggplot2::aes(x = segment, y = value, group = heatcool) +
            ggplot2::geom_line(ggplot2::aes(color = heatcool)) +
            ggplot2::facet_wrap(subRegion ~ ., scales = 'free_y') +
            ggplot2::ggtitle(paste0('HDCD at GCAM-USA Dispatch Segment in ', year_i)) +
            ggplot2::ylab('Degree-Hours') +
            ggplot2::scale_color_manual(values = c('heat' = '#1AB2FF',
                                                   'cool' = '#E61A33')) +
            ggplot2::scale_x_discrete(drop = FALSE) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                               vjust = 0.5))

          # create plot name
          filename_diagnostics_i <- file.path(
            folder_diagnostics,
            helios::create_name(c('segment', year_i, name_append), 'png'))

          # save plot
          ggplot2::ggsave(p,
                          filename = filename_diagnostics_i,
                          width = 25,
                          height = 15)

          print(paste0('Diagnostic figure saved as : ', filename_diagnostics_i))
        }

        # combined years with color gradients with free scale
        if(length(unique(hdcd_comb_diagnostics$year)) > 1){
          data_plot <- hdcd_comb_diagnostics %>%
            dplyr::mutate(segment = factor(segment, levels = segment_levels))

          n_color <- length(unique(hdcd_comb_diagnostics$year))
          pal_hd <- colorRampPalette(RColorBrewer::brewer.pal(9, 'YlOrRd'))
          pal_cd <-  colorRampPalette(RColorBrewer::brewer.pal(9, 'YlGnBu'))
          pal <- c(rev(pal_hd(n_color)), rev(pal_cd(n_color)))

          for (scale_i in c('free_y', 'fixed')){

            scale_name <- dplyr::case_when(scale_i == 'free_y' ~ 'freeScale',
                                           scale_i == 'fixed' ~ 'fixedScale')

            # plot
            p <- ggplot2::ggplot(data = data_plot) +
              ggplot2::geom_line(ggplot2::aes(x = segment, y = value,
                                              group = interaction(year, heatcool),
                                              color = interaction(year, heatcool))) +
              ggplot2::facet_wrap(subRegion ~ ., scales = scale_i) +
              ggplot2::ggtitle(paste0('HDCD at GCAM-USA Dispatch Segment for ', hdcd_comb_year_range)) +
              ggplot2::ylab('Degree-Hours') +
              ggplot2::scale_color_manual(values = pal,
                                          guide = ggplot2::guide_legend(title = 'HDCD (All Years)')) +
              ggplot2::scale_x_discrete(drop = FALSE) +
              ggplot2::theme_bw() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                 vjust = 0.5))

            # create plot name
            filename_diagnostics_i <- file.path(
              folder_diagnostics,
              helios::create_name(c('segment_allYears', scale_name, name_append), 'png'))

            # save plot
            ggplot2::ggsave(p,
                            filename = filename_diagnostics_i,
                            width = 25,
                            height = 15)
            print(paste0('Diagnostic figure saved as : ', filename_diagnostics_i))

          }

        }

        # combined years with color gradients with fixed scale
        if(length(unique(hdcd_comb_diagnostics$year)) > 1){
          data_plot <- hdcd_comb_diagnostics %>%
            dplyr::mutate(segment = factor(segment, levels = segment_levels))

          n_color <- length(unique(hdcd_comb_diagnostics$year))
          pal_hd <- colorRampPalette(RColorBrewer::brewer.pal(9, 'YlOrRd'))
          pal_cd <-  colorRampPalette(RColorBrewer::brewer.pal(9, 'YlGnBu'))
          pal <- c(rev(pal_hd(n_color)), rev(pal_cd(n_color)))


        }

      } else {
        message(paste0('Data is less than ', min_diagnostic_months, ' months. No diagnostic plots by dispatch segment are created.'))
      } # end of if(length(unique(hdcd_segment$segment)) >= min_diagnostic_months * 2)

    } else {
      message('hdcd input is not by dispatch segment. Skip diagnostics for segment.')
    } # end of if(any(unique(hdcd_segment$segment) %in% segment_levels))


    } else {

      message('No segment data provided. Skip segment diagnostics.')

    } # end of if(nrow(hdcd_segment) > 0)


    # --------------------------------------
    # By Month (compare against NOAA if States exist)
    # --------------------------------------

    if(all(nrow(hdcd_monthly) > 0, 'month' %in% names(hdcd_monthly))){

      n_months <- length(unique(hdcd_monthly$month))
      hdcd_monthly_year_range <- paste(unique(c(min(unique(hdcd_monthly$year)),
                                              max(unique(hdcd_monthly$year)))),
                                       collapse = '-')

      # only plot when there are more than 4 months
      if(n_months >= min_diagnostic_months) {

        # data frame for month
        months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
        monthNums <- c('01','02','03','04','05','06','07','08','09','10','11','12')
        monthsShort <- c('JA','FB','MR','AP','MY','JN','JL','AG','SP','OC','NV','DC')
        months_df <- data.frame(month = months,
                                monthNums = monthNums,
                                monthsShort = monthsShort)

        # create data frame with all combinations to fill in later
        all <- hdcd_monthly %>%
          tidyr::expand(subRegion, year, month, HDCD)

        # join data to the structure and fill in na values
        hdcd_monthly <- all %>%
          dplyr::left_join(hdcd_monthly %>%
                             dplyr::select(subRegion, year, month, HDCD, value)) %>%
          tidyr::replace_na(list(value = 0))

        # join month data frame
        hdcd_comb_monthly_diagnostics <- hdcd_monthly %>%
          dplyr::mutate(month = as.character(month),
                        month = dplyr::if_else(month == '1', '01', month),
                        month = dplyr::if_else(month == '2', '02', month),
                        month = dplyr::if_else(month == '3', '03', month),
                        month = dplyr::if_else(month == '4', '04', month),
                        month = dplyr::if_else(month == '5', '05', month),
                        month = dplyr::if_else(month == '6', '06', month),
                        month = dplyr::if_else(month == '7', '07', month),
                        month = dplyr::if_else(month == '8', '08', month),
                        month = dplyr::if_else(month == '9', '09', month),
                        year = as.character(year)) %>%
          dplyr::select(subRegion, year, monthNums = month, HDCD, value) %>%
          unique() %>%
          dplyr::mutate(value = abs(value),
                        scenario = 'ncdf') %>%
          dplyr::left_join(months_df, by = c('monthNums'))

        # Find if there are regions matching NOAA regions
        subRegions <- unique(hdcd_comb_monthly_diagnostics$subRegion)
        subRegions_noaa <- unique(helios::noaa_hddcdd$stateCode)
        use_noaa <- any(subRegions %in% subRegions_noaa)

        # Find closest matching years
        current_years <- as.integer(unique(hdcd_monthly$year))
        noaa_years <- as.integer(unique(helios::noaa_hddcdd$year))

        if(use_noaa){

          # If using NOAA data, append noaa to the output file name
          noaa_name <- paste('noaa', min(noaa_years), max(noaa_years), sep = '-')

          # bind NOAA data to include years occurred in outputs and the latest noaa data year
          hdcd_comb_monthly_diagnostics <- hdcd_comb_monthly_diagnostics %>%
            dplyr::bind_rows(
              helios::noaa_hddcdd %>%
                dplyr::select(subRegion = stateCode, year, month, HDCD = HDDCDD, value) %>%
                dplyr::filter(year %in% c(current_years, intersect(current_years, noaa_years), max(noaa_years))) %>%
                dplyr::mutate(scenario = 'noaa',
                              year = as.character(year),
                              HDCD = gsub('HDD', 'HD', HDCD),
                              HDCD = gsub('CDD', 'CD', HDCD)) %>%
                dplyr::left_join(months_df, by = c('month')))

          # for all years
          noaa_year_latest <- noaa_years[length(noaa_years)]

          # get NOAA data of the latest year
          noaa_latest <- hdcd_comb_monthly_diagnostics %>%
            dplyr::filter((year == noaa_year_latest & scenario == 'noaa')) %>%
            dplyr::rename(noaa = value) %>%
            dplyr::select(subRegion, month, HDCD, noaa)

          # combine calculated hdcd and NOAA data
          hdcd_comb_monthly_diagnostics_all <- hdcd_comb_monthly_diagnostics %>%
            dplyr::filter(scenario == 'ncdf') %>%
            dplyr::left_join(noaa_latest, by = c('subRegion', 'month', 'HDCD')) %>%
            dplyr::mutate(noaa = dplyr::if_else(is.na(noaa), 0, noaa),
                          month = factor(month, levels = months)) %>%
            dplyr::select(subRegion, scenario, year, month, HDCD, value, noaa)

        } else {
          noaa_name <- ''

          hdcd_comb_monthly_diagnostics_all <- hdcd_comb_monthly_diagnostics %>%
            dplyr::filter(scenario == 'ncdf') %>%
            dplyr::mutate(month = factor(month, levels = months))
        } # end of if(use_noaa)


        # write monthly outputs
        filename_monthly_diagnostics <- file.path(
          folder_diagnostics,
          helios::create_name(c('monthly', hdcd_monthly_year_range, noaa_name, name_append), 'csv'))

        # save plot data for monthly diagnostic
        data.table::fwrite(x = hdcd_comb_monthly_diagnostics,
                           file = filename_monthly_diagnostics)
        print(paste0('Diagnostic table saved as : ', filename_monthly_diagnostics))

        # Individual Years
        for(year_i in current_years) {

          if(use_noaa) {
            noaa_year_i <- noaa_years[which(abs(noaa_years - year_i) == min(abs(noaa_years - year_i)))]
            year_sel <- unique(c(year_i, noaa_year_i))

            noaa_name <- paste('NOAA', noaa_year_i, sep = '-')

          } else {
            year_sel <- year_i
          }

          # filter year and format for plotting
          hdcd_comb_monthly_diagnostics_i <- hdcd_comb_monthly_diagnostics %>%
            dplyr::filter(year %in% year_sel) %>%
            dplyr::mutate(scenario = paste0(scenario, '_', year),
                          scenario_hdcd = paste0(scenario, '_', HDCD),
                          month = factor(month, levels = months)) %>%
            dplyr::select(subRegion, scenario, scenario_hdcd, year, month, HDCD, value)

          # plot
          p <- ggplot2::ggplot(data = hdcd_comb_monthly_diagnostics_i,
                          ggplot2::aes(x = month, y = value, group = scenario_hdcd)) +
            ggplot2::geom_line(ggplot2::aes(color = HDCD, linetype = scenario),
                               linewidth = 1) +
            ggplot2::facet_wrap(subRegion ~ ., scales = 'free_y') +
            ggplot2::ggtitle(paste0('NCDF-', year_i, ' VS ', noaa_name)) +
            ggplot2::ylab('Monthly Degree-Days') +
            ggplot2::scale_color_manual(values = c('HD' = '#1AB2FF',
                                                   'CD' = '#E61A33')) +
            ggplot2::scale_linetype_manual(values = c(1, 2)) +
            ggplot2::scale_x_discrete(drop = FALSE) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                               vjust = 0.5))

          # create file name to save
          filename_monthly_diagnostics_i <- file.path(
            folder_diagnostics,
            helios::create_name(c('monthly', year_i, tolower(noaa_name), name_append), 'png'))

          # save plot
          ggplot2::ggsave(p,
                          filename =  filename_monthly_diagnostics_i,
                          width = 25,
                          height = 15) # save plot

          print(paste0('Diagnostic figure saved as : ', filename_monthly_diagnostics_i))
        }

        # Plot All Years Together
        if(length(unique(hdcd_comb_monthly_diagnostics_all$year)) > 1) {

          # create palette
          n_color <- length(unique(hdcd_comb_monthly_diagnostics_all$year))
          pal_hd <- colorRampPalette(RColorBrewer::brewer.pal(9, 'YlOrRd'))
          pal_cd <-  colorRampPalette(RColorBrewer::brewer.pal(9, 'YlGnBu'))
          pal <- c(rev(pal_hd(n_color)), rev(pal_cd(n_color)))

          # create file name to save
          filename_monthly_diagnostics_all <- file.path(
            folder_diagnostics,
            helios::create_name(c('monthly_allYears', tolower(noaa_name), name_append), 'png'))

          # plot
          p <- ggplot2::ggplot(data = hdcd_comb_monthly_diagnostics_all) +
            ggplot2::geom_line(ggplot2::aes(x = month, y = value,
                                            group = interaction(subRegion, year, HDCD),
                                            color = interaction(year, HDCD))) +
            {
              if(use_noaa) {
                ggplot2::geom_line(ggplot2::aes(x = month, y = noaa, group = HDCD),
                                   color = 'black', linewidth = 1.2, linetype = 'dashed')
              }
            } +
            ggplot2::facet_wrap(subRegion ~ ., scales = 'free_y') +
            {
              if(use_noaa) {
                ggplot2::ggtitle(paste0('NCDF (all years) VS', ' NOAA (', noaa_year_latest, ')'))
              } else {
                ggplot2::ggtitle('NCDF (all years)')
              }
            } +
            ggplot2::ylab('Monthly Degree-Days') +
            ggplot2::scale_color_manual(values = pal,
                                        guide = ggplot2::guide_legend(title = 'NCDF (All Years)')) +
            ggplot2::scale_x_discrete(drop = FALSE) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                               vjust = 0.5))

          # save plot
          ggplot2::ggsave(p,
                          filename = filename_monthly_diagnostics_all,
                          width = 25,
                          height = 15)
          print(paste0('Diagnostic figure saved as : ', filename_monthly_diagnostics_all))

        } # end of if(length(unique(hdcd_comb_monthly_diagnostics_all$year)) > 1)


      } else {
        message(paste0('Data is less than ', min_diagnostic_months, ' months. No diagnostic plots by monthly time step are created.'))
      } # end of if(n_months >= min_diagnostic_months)


    } else {

      message('No monthly data provided. Skip mothly diagnostics.')

    }# end of if(nrow(hdcd_monthly) > 0)

    #...............

    print('Diagnostics complete.')

  }
