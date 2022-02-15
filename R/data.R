# Internal Data


#.................................
# segment_map_utc
#................................

#' Segment map in UTC by state, segment, month, day, hour
#'
#' @source gcamdata/outputs/L102.date_load_curve_mapping_S_gcamusa.csv.
#' @format R data.frame
#' @examples
#' \dontrun{
#'  library(helios);
#'  segment_map_utc <- helios::segment_map_utc
#' }
"segment_map_utc"

#.................................
# L2441.HDDCDD_Fixed_gcamusa_seg
#................................

#' L2441.HDDCDD_Fixed_gcamusa_seg
#'
#' @source gcamdata/outputs/L2441.HDDCDD_Fixed_gcamusa.csv
#' @format R dataframe
#' @examples
#' \dontrun{
#'  library(helios);
#'  L2441.HDDCDD_Fixed_gcamusa_seg <- helios::L2441.HDDCDD_Fixed_gcamusa_Seg
#' }
"L2441.HDDCDD_Fixed_gcamusa_seg"

#.................................
# Copy of rmap::mapUS52 for local use in helios
#................................

#' Copy of rmap::mapUS52 for local use in helios
#'
#' @source rmap::mapUS52
#' @format SpatialPolygonsDataFrame
#' @examples
#' \dontrun{
#'  library(helios);
#'  mapUS52 <- helios::mapUS52
#' }
"mapUS52"

#.................................
# NOAA HDDCDD data
#................................

#' noaa_hddcdd
#'
#' @source https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/
#' @format dataframe
#' @examples
#' \dontrun{
#'  library(helios);
#'  noaa_hddcdd <- helios::noaa_hddcdd
#' }
"noaa_hddcdd"

#.................................
# Pre-processed Population Files
#................................

# #' population_conus_total_ssp3_2020_2100_wrf_wgs84
# #'
# #' @source Processed by Chris Vernon from:
# #' Paper: Jiang, L., O’Neill, B. C., Zoraghein, H., & Dahlke, S. (2020). Population scenarios for US states consistent with shared socioeconomic pathways. Environmental Research Letters, 15(9), 094097. https://iopscience.iop.org/article/10.1088/1748-9326/aba5b1
# #' Data: https://zenodo.org/record/3956412#.YgqKOd_MJPZ
# #' Processing Scripts: https://zenodo.org/record/3956703#.YgqKPt_MJPZ
# #' @format dataframe
# #' @examples
# #' \dontrun{
# #'  library(helios);
# #'  population_conus_total_ssp3_2020_2100_wrf_wgs84 <-
# #'  helios::population_conus_total_ssp3_2020_2100_wrf_wgs84
# #' }
# "population_conus_total_ssp3_2020_2100_wrf_wgs84"

# #' population_conus_total_ssp5_2020_2100_wrf_wgs84
# #'
# #' @source Processed by Chris Vernon from:
# #' Paper: Jiang, L., O’Neill, B. C., Zoraghein, H., & Dahlke, S. (2020). Population scenarios for US states consistent with shared socioeconomic pathways. Environmental Research Letters, 15(9), 094097. https://iopscience.iop.org/article/10.1088/1748-9326/aba5b1
# #' Data: https://zenodo.org/record/3956412#.YgqKOd_MJPZ
# #' Processing Scripts: https://zenodo.org/record/3956703#.YgqKPt_MJPZ
# #' @format dataframe
# #' @examples
# #' \dontrun{
# #'  library(helios);
# #'  population_conus_total_ssp5_2020_2100_wrf_wgs84 <-
# #'  helios::population_conus_total_ssp5_2020_2100_wrf_wgs84
# #' }
# "population_conus_total_ssp5_2020_2100_wrf_wgs84"