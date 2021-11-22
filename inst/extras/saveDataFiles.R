
library(tibble);library(dplyr);library(rgdal);library(devtools);library(rmap); library(raster);
library(lubridate)

#-----------------
# Initialize
#-----------------

gcamdata_folder = "C:/Z/models/GCAMVersions/gcam-usa-im3/input/gcamdata"

#-----------------
# Segment Map UTC for WRF data
#-----------------

# WRF data is in UTC
# GCAM segment data is in ETC

L102.date_load_curve_mapping_S_gcamusa <- read.csv(paste0(gcamdata_folder,"/outputs/L102.date_load_curve_mapping_S_gcamusa.csv"), comment.char = "#") %>% tibble::as_tibble()

# Convert GCAM segment data to UTC
segment_map_utc <- L102.date_load_curve_mapping_S_gcamusa %>%
  dplyr::mutate(
    date = gsub("Z","",gsub("T"," ",date)),
    date = as.POSIXct(date, tz="EST"),
    date = lubridate::force_tz(date,tz="EST"),
    date = lubridate::with_tz(date,tz="UTC")) %>%
  dplyr::mutate(year = substr(date,1,4),
                segment = dplyr::if_else(is_super_peak,"superpeak",paste0(month,"_",day_night)),
                month = substr(date,6,7),
                day = substr(date,9,10),
                hour = substr(date,12,13)) %>%
  dplyr::select(subRegion=state,segment, month, day, hour); segment_map_utc

use_data(segment_map_utc, version=3, overwrite=T)

#-----------------
# L2441.HDDCDD_Fixed_gcamusa
#-----------------

L2441.HDDCDD_Fixed_gcamusa <- read.csv(paste0(gcamdata_folder,"/outputs/L2441.HDDCDD_Fixed_gcamusa.csv"), comment.char = "#") %>% tibble::as_tibble()
thermal.building.services <- gsub(paste0(" ",(helios::segment_map_utc$segment) %>% unique(),collapse="|"),
                          "",
                          L2441.HDDCDD_Fixed_gcamusa$thermal.building.service.input%>%unique()) %>% unique(); thermal.building.services

L2441.HDDCDD_Fixed_gcamusa_seg <- L2441.HDDCDD_Fixed_gcamusa %>%
  dplyr::select(subRegion=region, gcam.consumer, nodeInput, building.node.input, thermal.building.service.input) %>%
  dplyr::mutate(segment = gsub(paste0(thermal.building.services," ",collapse="|"),
                               "",
                               thermal.building.service.input)) %>%
  unique(); L2441.HDDCDD_Fixed_gcamusa_seg

use_data(L2441.HDDCDD_Fixed_gcamusa_seg, version=3, overwrite=T)

#-----------------
# US52 map from Rmap saved locally to remove dependency
#-----------------

mapUS52 <- rmap::mapUS52
use_data(mapUS52, version=3, overwrite=T)

