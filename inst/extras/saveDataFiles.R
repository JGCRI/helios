
library(tibble);library(dplyr);library(rgdal);library(devtools);library(rmap); library(raster);
library(lubridate); library(httr); library(ggplot2); library(rchart)

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
    tmp<-read.fwf(file = url_i, skip=15,nrows=48,widths = c(17, 6, 1000)) %>%
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
    tmp<-read.fwf(file = url_i, skip=15,nrows=48,widths = c(17, 6, 1000)) %>%
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



