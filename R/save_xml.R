#' save_xml
#'
#' Heating and Cooling Degree diagnostic scripts
#'
#' @param hdcd Default = tibble::tibble()
#' @param folder Default = paste0(getwd()).
#' @param filename Default = "hdcd_diagnostic".
#' @param name_append Default = "". Name to append to all filenames
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

save_xml <- function(hdcd = tibble::tibble(),
                     folder = paste0(getwd()),
                     filename = "hdcd_xml",
                     name_append = "") {

  print("Starting function save_xml...")

  #......................
  # Initialize
  #......................

  if(T){

  NULL -> subRegion -> gcam.consumer -> nodeInput -> value ->
      building.node.input -> thermal.building.service.input -> year

  }

  #......................
  # save_xml
  #......................

  # Format to match GCAM output file L2441.HDDCDD_Fixed_rcp4p5_gcamusa.csv
  if(nrow(hdcd)>0){
    hdcd_comb_xml <- hdcd %>%
      dplyr::select(region=subRegion,
                    gcam.consumer,
                    nodeInput,
                    building.node.input,
                    thermal.building.service.input,
                    year,
                    degree.days=value)

    filename_i_xml <- paste0(folder, "/", basename(gsub(".csv", "", filename)),name_append,".xml")

    gcamdata::create_xml(filename_i_xml) %>%
      gcamdata::add_xml_data(hdcd_comb_xml, "HDDCDD")%>%
      gcamdata::run_xml_conversion()

    print(paste0("File saved as : ", filename_i_xml))
  }

    print("save_xml complete.")

  }
