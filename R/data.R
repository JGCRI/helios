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
