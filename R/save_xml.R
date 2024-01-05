#' save_xml
#'
#' Heating and Cooling Degree diagnostic scripts
#'
#' @param hdcd_gcam Default = tibble::tibble(). hdcd output with GCAM required format
#' @param folder Default = file.path(getwd(), 'output').
#' @param filename Default = 'hdcd_diagnostic'.
#' @param name_append Default = ''. Name to append to all filenames
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

save_xml <- function(hdcd_gcam = tibble::tibble(),
                     folder = file.path(getwd(), 'output'),
                     filename = 'hdcd',
                     name_append = '') {
  print('Starting function save_xml...')

  # ......................
  # Initialize
  # ......................

  if (T) {
    NULL -> subRegion -> gcam.consumer -> nodeInput -> value ->
    building.node.input -> thermal.building.service.input -> year
  }

  # ......................
  # save_xml
  # ......................

  # Format to match GCAM output file L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv
  if (nrow(hdcd_gcam) > 0) {
    hdcd_comb_xml <- hdcd_gcam %>%
      dplyr::mutate(value = abs(value),
                    value = round(value, 2)) %>%
      dplyr::select(
        region = subRegion,
        gcam.consumer,
        nodeInput,
        building.node.input,
        thermal.building.service.input,
        year,
        degree.days = value) %>%
      tibble::as_tibble()

    # Save csv corresponding to xml
    filename_i_xml_csv <- file.path(
      folder,
      helios::create_name(c(basename(gsub('.csv', '', filename)), name_append, 'xml'), 'csv'))
    data.table::fwrite(hdcd_comb_xml, file = filename_i_xml_csv)
    print(paste0('File saved as : ', filename_i_xml_csv))

    filename_i_xml <- file.path(
      folder,
      helios::create_name(c(basename(gsub('.csv', '', filename)), name_append), 'xml'))

    gcamdata::create_xml(filename_i_xml) %>%
      gcamdata::add_xml_data(hdcd_comb_xml, 'HDDCDD') %>%
      gcamdata::run_xml_conversion()

    print(paste0('File saved as : ', filename_i_xml))
  }

  print('save_xml complete.')
}
