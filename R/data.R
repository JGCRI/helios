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
# L244.HDDCDD_building
#................................

#' L244.HDDCDD_building
#'
#' @source gcamdata/outputs/L244.HDDCDD_constdd_no_GCM.csv
#' @format R dataframe
#' @examples
#' \dontrun{
#'  library(helios);
#'  L244.HDDCDD_building <- helios::L244.HDDCDD_building
#' }
"L244.HDDCDD_building"


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
# WRF Grid to US49 Mapping
#................................

#' mapping_wrf_us49
#'
#' @source wrf climate data wrfout_d01_2020-01-01_01%3A00%3A00.nc
#' @format tibble
#' @examples
#' \dontrun{
#'  library(helios);
#'  mapping_wrf_us49 <- helios::mapping_wrf_us49
#' }
"mapping_wrf_us49"

#.................................
# 0.5 Grid to GCAM 32 region Mapping
#................................

#' mapping_grid_region
#'
#' @source rmap::mapping_tethys_grid_basin_region_country
#' @format tibble
#' @examples
#' \dontrun{
#'  library(helios);
#'  mapping_grid_region <- helios::mapping_grid_region
#' }
"mapping_grid_region"

#.................................
# WRF Example Data
#.................................

#' example_wrf_usa_ncdf
#'
#' @source .inst/extras/wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc
#' @format list
#' @examples
#' \dontrun{
#'  library(helios);
#'  example_wrf_usa_ncdf <- helios::example_wrf_usa_ncdf
#' }
"example_wrf_usa_ncdf"

#.................................
# CMIP6 Example Data
#.................................

#' example_cmip6_china_ncdf
#'
#' @source .inst/extras/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc
#' @format list
#' @examples
#' \dontrun{
#'  library(helios);
#'  example_cmip6_china_ncdf <- helios::example_cmip6_china_ncdf
#' }
"example_cmip6_china_ncdf"

#.................................
# Population NetCDF Example Data
#.................................
#' example_pop_china_ncdf
#'
#' @source .inst/extras/ssp1_2020_sub.nc
#' @format list
#' @examples
#' \dontrun{
#'  library(helios);
#'  example_pop_china_ncdf <- helios::example_pop_china_ncdf
#' }
"example_pop_china_ncdf"

#.................................
# Population CSV Example Data
#.................................
#' example_pop_usa_csv
#'
#' @source .inst/extras/population_conus_ssp2_2020wrf_wgs84.csv
#' @format data.table
#' @examples
#' \dontrun{
#'  library(helios);
#'  example_pop_usa_csv <- helios::example_pop_usa_csv
#' }
"example_pop_usa_csv"

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
