
library(tibble)
library(dplyr)
library(rgdal)
library(devtools)
library(rmap)
library(metis)
library(raster)

library(lubridate)
library(httr)
library(ggplot2)
library(rchart)
library(usethis)

#-----------------
# Initialize
#-----------------

gcamdata_folder <- "C:/WorkSpace/GCAM-Models/gcam-usa-im3/input/gcamdata"

#-----------------
# Segment Map UTC with default super peak for WRF data
#-----------------

# WRF data is in UTC
# GCAM segment data is in ETC

# L102.date_load_curve_mapping_S_gcamusa <- read.csv(paste0(gcamdata_folder,"/outputs/L102.date_load_curve_mapping_S_gcamusa.csv"), comment.char = "#") %>% tibble::as_tibble()
# Modified load curve: day/night/super peak mapping is the same for states within the same grid region
L102.date_load_curve_mapping_S_gcamusa <-
  read.csv('C:/WorkSpace/IM3/gcam-usa/gcam-usa-im3-v5p3/gcamdata/outputs/L102.date_load_curve_mapping_S_gcamusa_modified.csv') %>%
  tibble::as_tibble()

# Convert GCAM segment data to UTC
segment_map_utc <- L102.date_load_curve_mapping_S_gcamusa %>%
  dplyr::mutate(
    date = gsub("Z", "", gsub("T", " ", date)),
    date = as.POSIXct(date, tz = "EST"),
    date = lubridate::force_tz(date, tz = "EST"),
    date = lubridate::with_tz(date, tz = "UTC")) %>%
  dplyr::mutate(year = substr(date, 1, 4),
                segment = dplyr::if_else(is_super_peak, "superpeak", paste0(month, "_", day_night)),
                month = substr(date, 6, 7),
                day = substr(date, 9, 10),
                hour = substr(date, 12, 13)) %>%
  dplyr::select(subRegion = state, segment, month, day, hour); segment_map_utc

use_data(segment_map_utc, version = 3, overwrite = T)

#-----------------
# Segment Map UTC without super peak for WRF data
#-----------------

# WRF data is in UTC
# GCAM segment data is in ETC

# L102.date_load_curve_mapping_S_gcamusa <- read.csv(paste0(gcamdata_folder,"/outputs/L102.date_load_curve_mapping_S_gcamusa.csv"), comment.char = "#") %>% tibble::as_tibble()
# Modified load curve: day/night/super peak mapping is the same for states within the same grid region
L102.date_load_curve_mapping_S_gcamusa <-
  read.csv('C:/WorkSpace/IM3/gcam-usa/gcam-usa-im3-v5p3/gcamdata/outputs/L102.date_load_curve_mapping_S_gcamusa_modified.csv') %>%
  tibble::as_tibble()

# Convert GCAM segment data to UTC
segment_map_utc_no_superpeak <- L102.date_load_curve_mapping_S_gcamusa %>%
  dplyr::mutate(
    date = gsub("Z", "", gsub("T", " ", date)),
    date = as.POSIXct(date, tz = "EST"),
    date = lubridate::force_tz(date, tz = "EST"),
    date = lubridate::with_tz(date, tz = "UTC")) %>%
  dplyr::mutate(year = substr(date, 1, 4),
                segment = paste0(month, "_", day_night),
                month = substr(date, 6, 7),
                day = substr(date, 9, 10),
                hour = substr(date, 12, 13)) %>%
  dplyr::select(subRegion = state, segment, month, day, hour); segment_map_utc_no_superpeak

use_data(segment_map_utc_no_superpeak, version = 3, overwrite = T)

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

use_data(L2441.HDDCDD_Fixed_gcamusa_seg, version = 3, overwrite = T)

#-----------------
# L244.HDDCDD_constdd_no_GCM
#----------------

L244.HDDCDD_constdd_no_GCM <- read.csv(paste0(gcamdata_folder,"/outputs/L244.HDDCDD_constdd_no_GCM.csv"), comment.char = "#") %>% tibble::as_tibble()

L244.HDDCDD_building <- L244.HDDCDD_constdd_no_GCM %>%
  dplyr::select(-degree.days, -year) %>%
  unique() %>%
  dplyr::mutate(region = gsub('-', '_', region))

use_data(L244.HDDCDD_building, version = 3, overwrite = T)

#-----------------
# US52 map from Rmap saved locally to remove dependency
#-----------------

mapUS52 <- rmap::mapUS52
use_data(mapUS52, version=3, overwrite=T)

#-----------------
# NOAA Data
# Source: https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/
# Downloaded directly from source
#-----------------

# Prepare data for Comparison with NOAA data
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthNums = c("01","02","03","04","05","06","07","08","09","10","11","12")
monthsShort<- c("JA","FB","MR","AP","MY","JN","JL","AG","SP","OC","NV","DC")
monthsLong = c("January","February","March","April","May","June","July","August","September","October","November","December")
stateName=c("ALASKA","ALABAMA","ARIZONA","ARKANSAS","CALIFORNIA","COLORADO","CONNECTICUT",
             "DELAWARE","DISTRCTCOLUMBIA","FLORIDA","GEORGIA","HAWAII","IDAHO","ILLINOIS","INDIANA","IOWA",
             "KANSAS","KENTUCKY","LOUISIANA","MAINE","MARYLAND","MASSACHUSETTS","MICHIGAN","MINNESOTA",
             "MISSISSIPPI","MISSOURI","MONTANA","NEBRASKA","NEVADA","NEWHAMPSHIRE","NEWJERSEY","NEWMEXICO",
             "NEWYORK","NORTHCAROLINA","NORTHDAKOTA","OHIO","OKLAHOMA","OREGON","PENNSYLVANIA","RHODEISLAND",
             "SOUTHCAROLINA","SOUTHDAKOTA","TENNESSEE","TEXAS","UTAH","VERMONT","VIRGINIA","WASHINGTON","WESTVIRGINIA",
             "WISCONSIN","WYOMING")
US48<- c('AL','AZ','AR','CA','CO','CT','DE','FL','GA','ID','IL','IN','IA','KS','KY','LA',
         'ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
         'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
US50<- c('AK','AL','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA',
         'ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
         'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
stateCodes <- data.frame(stateName,stateCode=US50)
monthCodes <- data.frame(month=months,monthsShort)
monthCodesNum <- data.frame(month=monthNums,monthsShort)
years = c(2000:2021)

# NOAA Data
if(F){ # Reread data if needed
tbl<-tibble()
for (year_i in years){
  for (month_i in months){

    print(paste0("Reading data for year ",year_i," and month ",month_i,"..."))

    # Cooling
    url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Cooling%20Degree%20Days/monthly%20cooling%20degree%20days%20state/",year_i,"/",month_i,"%20",year_i,".txt")
    if(httr::GET(url_i)$status_code == "404"){
      url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Cooling%20Degree%20Days/monthly%20cooling%20degree%20days%20state/",year_i,"/",tolower(month_i),"%20",year_i,".txt")
      if(httr::GET(url_i)$status_code == "404"){
        month_i_long = monthsLong[which(months==month_i)]
        url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Cooling%20Degree%20Days/monthly%20cooling%20degree%20days%20state/",year_i,"/",tolower(month_i_long),"%20",year_i,".txt")
        if(httr::GET(url_i)$status_code == "404"){
          url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Cooling%20Degree%20Days/monthly%20cooling%20degree%20days%20state/",year_i,"/",tolower(month_i),",%20",year_i,".txt")
        }
        }
      }
    tmp<-read.fwf(file = url_i, skip=15,nrows=51,widths = c(17, 6, 1000)) %>%
      dplyr::mutate(stateName=gsub(" ","", V1, perl=T),
                    value=V2)%>%
      left_join(stateCodes, by="stateName")%>%
      dplyr::mutate(HDDCDD="CDD",
                    month=month_i,
                    year=year_i)%>%
      dplyr::select(stateName,stateCode,year,month,value,HDDCDD)%>%tibble::as_tibble(); tmp
    tbl<- bind_rows(tbl,tmp)

    # Heating

    url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Heating%20degree%20Days/monthly%20states/",year_i,"/",month_i,"%20",year_i,".txt")
    if(httr::GET(url_i)$status_code == "404"){
      url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Heating%20degree%20Days/monthly%20states/",year_i,"/",tolower(month_i),"%20",year_i,".txt")
      if(httr::GET(url_i)$status_code == "404"){
        month_i_long = monthsLong[which(months==month_i)]
        url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Heating%20degree%20Days/monthly%20states/",year_i,"/",tolower(month_i_long),"%20",year_i,".txt")
        if(httr::GET(url_i)$status_code == "404"){
          month_i_long = monthsLong[which(months==month_i)]
          url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Heating%20degree%20Days/monthly%20states/",year_i,"/",tolower(month_i),",%20",year_i,".txt")
          if(httr::GET(url_i)$status_code == "404"){
            # Specific Problem in Noaa data. 2002 Nov has data for 2003
            if(year_i==2002 & month_i=="Nov"){year_i_temp="2003"}
            url_i = paste0("https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/Heating%20degree%20Days/monthly%20states/",year_i,"/",tolower(month_i),"%20",year_i_temp,".txt")
          }
          }
        }
      }
    tmp<-read.fwf(file = url_i, skip=15,nrows=51,widths = c(17, 6, 1000)) %>%
      dplyr::mutate(stateName=gsub(" ","", V1, perl=T),
                    value=V2)%>%
      left_join(stateCodes, by="stateName")%>%
      dplyr::mutate(HDDCDD="HDD",
                    month=month_i,
                    year=year_i)%>%
      dplyr::select(stateName,stateCode,year,month,value,HDDCDD)%>%tibble::as_tibble(); tmp
    tbl<- bind_rows(tbl,tmp)
  }
  }

 noaa_hddcdd <- tbl%>%
  left_join(monthCodes, by="month")%>%
  dplyr::mutate(monthCode=monthsShort)%>%dplyr::select(-monthsShort)%>%
  dplyr::mutate(monthCode=factor(monthCode,levels=monthsShort)) %>%
  dplyr::mutate(month=factor(month,levels=months))

saveRDS(noaa_hddcdd,"./inst/extras/noaa_hddcdd.RDS")

}

noaa_hddcdd <- readRDS("./inst/extras/noaa_hddcdd.RDS")
use_data(noaa_hddcdd, version=3, overwrite=T)

#----------------------
# Selected year HDD CDD Monthly RG
#-----------------------
year_i = 2005
ggplot2::ggplot(data=noaa_hddcdd %>% dplyr::filter(year==year_i),
                aes(x=month,
                    y=value,
                    group=HDDCDD)) +
  ggplot2::geom_line(aes(color=HDDCDD)) +
  ggplot2::scale_color_manual(values=c("HDD"="indianred1","CDD"="dodgerblue")) +
  ggplot2::facet_wrap(.~stateCode) +
  ggplot2::labs(title=paste0(year_i,"NOAA Population Weighted HDD CDD by State"),
                subtitle = "Source: https://ftp.cpc.ncep.noaa.gov/htdocs/products/analysis_monitoring/cdus/degree_days/archives/")+
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./inst/extras/noaa_hddcdd.png",width=13,height=10)
print(paste0("Plot saved as ",getwd(),"/inst/extras/noaa_hddcdd.png"))

#----------------------
# Population Files
#-----------------------

population_conus_total_ssp3_2020_2100_wrf_wgs84 <- data.table::fread("C:/Z/projects/current/00_IM3/tests/process_hdhcdh/pop_1km/population_conus_total_ssp3_2020-2100_wrf_wgs84.csv") %>%
  tibble::as_tibble(); population_conus_total_ssp3_2020_2100_wrf_wgs84
use_data(population_conus_total_ssp3_2020_2100_wrf_wgs84, version=3, overwrite=T)

population_conus_total_ssp5_2020_2100_wrf_wgs84 <- data.table::fread("C:/Z/projects/current/00_IM3/tests/process_hdhcdh/pop_1km/population_conus_total_ssp5_2020-2100_wrf_wgs84.csv") %>%
  tibble::as_tibble(); population_conus_total_ssp5_2020_2100_wrf_wgs84
use_data(population_conus_total_ssp5_2020_2100_wrf_wgs84, version=3, overwrite=T)


#----------------------
# Map WRF grid to US states
#-----------------------
library(raster)
library(sf)
library(dplyr)
library(ncdf4)
library(rmap)
library(usethis)

# Path to ncdf file
# Source:
ncdf_file <- "C:/WorkSpace/IM3/helios/example_nersc_data/wrfout_d01_2020-01-01_01%3A00%3A00.nc"

# Read ncdf file
ncdf <- ncdf4::nc_open(ncdf_file)

# Load shapefile from helios dataset
shape <- rmap::mapUS49

# Assign IDs to subRegions
nam <- unique(shape$subRegion)
nam_df <- data.frame(ID = 1:length(nam), subRegion = nam)

# Get raster brick for Temperature
ncdf_brick <- raster::brick(ncdf_file, varname = 'T2', ncdf = TRUE)
ncdf_ras <- ncdf_brick[[1]] # Base raster
ncdf_lat <- (raster::brick(ncdf_file, varname = 'XLAT', ncdf = TRUE))[[1]]
ncdf_lon <- (raster::brick(ncdf_file, varname = 'XLONG', ncdf = TRUE))[[1]]


# Convert raster in to an sf object ============================================
# Step 0: Get Lat long
ncdf_lat_df <- raster::as.data.frame(ncdf_lat, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(lat = X1); ncdf_lat_df %>% head()
ncdf_lon_df <- as.data.frame(ncdf_lon, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(lon = X1); ncdf_lon_df %>% head()

# Step 1: convert to a table with lat, lon, and z
ncdf_ras_df <- raster::as.data.frame(ncdf_ras, xy = TRUE, na.rm = TRUE) %>%
  dplyr::rename(z = X1) %>%
  dplyr::left_join(ncdf_lat_df, by=c("x","y")) %>%
  dplyr::left_join(ncdf_lon_df, by=c("x","y")) %>%
  dplyr::select(lat,lon,z) %>%
  dplyr::mutate(latx = lat,
                lonx = lon); ncdf_ras_df %>% head()

# Step 2: convert to sf object using sf::st_as_sf
ncdf_sf <- ncdf_ras_df %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapping_wrf_us49 <- sf::st_intersection(ncdf_sf, shape)
mapping_wrf_us49 <- mapping_wrf_us49 %>%
  tibble::as_tibble() %>%
  dplyr::select(region, subRegion, lat = latx, lon = lonx) %>%
  dplyr::mutate(across(c(lat, lon), ~round(., 5))) %>%
  dplyr::left_join(nam_df, by = 'subRegion')

usethis::use_data(mapping_wrf_us49, overwrite=T)


#----------------------
# Map 0.5 grid to GCAM regions
#-----------------------
mapping_grid_region <- rmap::mapping_tethys_grid_basin_region_country %>%
  dplyr::select(lat, lon, region = regionName, subRegion = regionName, ID = regionID)

usethis::use_data(mapping_grid_region, overwrite=T)

#----------------------
# Map 0.5 grid to GCAM 32 regions with 52 US states (all including AK, HI, DC and PR)
#-----------------------
gridTable <- data.frame(lat = rmap::mapping_tethys_grid_basin_region_country$lat,
                        lon = rmap::mapping_tethys_grid_basin_region_country$lon)

GCAMReg32US52_grid <- metis::metis.gridByPoly(gridTable = gridTable,
                                              shape = metis::mapGCAMReg32US52,
                                              colName = 'subRegion')

mapping_grid_region_US52 <- GCAMReg32US52_grid %>%
  dplyr::group_by(lat, lon) %>%
  dplyr::mutate(max = max(gridCellAreaRatio)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(gridCellAreaRatio == max) %>%
  dplyr::select(lat, lon, subRegion) %>%
  dplyr::left_join(rmap::mapping_tethys_grid_basin_region_country %>%
                     dplyr::select(lat, lon, ID = regionID, region = regionName),
                   by = c('lat', 'lon')) %>%
  dplyr::mutate(subRegion = dplyr::if_else(region == 'Taiwan' & subRegion == 'China',
                                           'Taiwan', subRegion),
                region = dplyr::if_else(subRegion %in% us_states & region != 'USA',
                                        'USA', region),
                ID = ifelse(region == 'USA', 1, ID))

usethis::use_data(mapping_grid_region_US52, overwrite = T)

#-----------------------
# WRF example data
#-----------------------
f_wrf_usa_ncdf<- system.file(
  'extras',
  'wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc',
  package = 'helios')
example_wrf_usa_ncdf <- ncdf4::nc_open(f_wrf_usa_ncdf)
usethis::use_data(example_wrf_usa_ncdf, version=3, overwrite=T)

#-----------------------
# CMIP6 example data
#-----------------------
f_cmip6_china_ncdf <- system.file(
  'extras',
  'gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc',
  package = 'helios')
example_cmip6_china_ncdf <- ncdf4::nc_open(f_cmip6_china_ncdf)
usethis::use_data(example_cmip6_china_ncdf, version=3, overwrite=T)

#--------------------------------
# Population NetCDF Example Data
#--------------------------------
f_pop_china_ncdf <- system.file(
  'extras',
  'ssp1_2020_sub.nc',
  package = 'helios')
example_pop_china_ncdf <- ncdf4::nc_open(f_pop_china_ncdf)
usethis::use_data(example_pop_china_ncdf, version = 3, overwrite = T)

#--------------------------------
# Population CSV Example Data
#--------------------------------
f_pop_usa_csv <- system.file(
  'extras',
  'population_conus_ssp2_2020wrf_wgs84.csv',
  package = 'helios')
example_pop_usa_csv <- data.table::fread(f_pop_usa_csv)
usethis::use_data(example_pop_usa_csv, version = 3, overwrite = T)


#--------------------------------
# HDCD Example Data by Segment
#--------------------------------

example_hdcd_segment_usa <- data.table::fread('inst/extras/hdcd_diagnostic_2020-2100rcp45cooler_ssp3.csv') %>%
  dplyr::rename(HDCD = heatcool) %>%
  dplyr::mutate(HDCD = gsub('heat', 'HD', HDCD),
                HDCD = gsub('cool', 'CD', HDCD),
                value = dplyr::if_else(HDCD == 'HD', -value, value)) %>%
  dplyr::filter(year %in% seq(2020, 2050, 5))
usethis::use_data(example_hdcd_segment_usa, version = 3, overwrite = T)


#--------------------------------
# HDCD Example Data by Month
#--------------------------------

example_hdcd_monthly_usa <- data.table::fread('inst/extras/monthly_ncdf_2020-2100_noaa_2000-2021rcp45cooler_ssp3.csv') %>%
  dplyr::filter(year %in% seq(2020, 2050, 5),
                scenario %in% 'ncdf') %>%
  dplyr::select(subRegion, year, month = monthNums, HDCD = HDDCDD, value) %>%
  dplyr::mutate(HDCD = gsub('HDD', 'HD', HDCD),
                HDCD = gsub('CDD', 'CD', HDCD),
                value = dplyr::if_else(HDCD == 'HD', -value, value))
usethis::use_data(example_hdcd_monthly_usa, version = 3, overwrite = T)


#--------------------------------
# Available Spatial Options
#--------------------------------

spatial_options <- tibble::tribble(
  ~spatial, ~description,
  'gcam_us49', '49 U.S. States including D.C.and excluding Hawaii and Alaska',
  'gcam_regions32', 'Global 32 GCAM Regions',
  'gcam_regions31_us52', 'Global 31 GCAM Regions (excluding USA as one region) + 52 U.S. States including D.C, Puerto Rico',
  'gcam_countries', 'Global 240 countries',
  'gcam_basins', 'Global 235 GCAM water basins'
)
usethis::use_data(spatial_options, version = 3, overwrite = T)


#--------------------------------
# State to Grid Region Mapping
#--------------------------------
# read state to grid region mapping from gcamdata
mapping_states_gridregion <- data.table::fread(
  file.path(gcamdata_folder, 'inst/extdata/gcam-usa/states_subregions.csv')) %>%
  dplyr::select(subRegion = state, grid_region)

usethis::use_data(mapping_states_gridregion, version = 3, overwrite = T)
