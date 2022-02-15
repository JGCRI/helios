library(helios); library(dplyr); library(raster)

#................
# Initialize
#...............

ncdf_i = c("C:/Z/projects/current/00_IM3/tests/process_hdhcdh/wrfout_d01_1979-01-01_00%3A00%3A00",
           "C:/Z/projects/current/00_IM3/tests/process_hdhcdh/wrfout_d01_1994-12-17_01%3A00%3A00.nc")
spatial_i = "gcamusa"
temporal_i = NULL # "gcamusa"
#population_i = helios::population_conus_total_ssp5_2020_2100_wrf_wgs84
population_i = "C:/Z/projects/current/00_IM3/tests/process_hdhcdh/pop_1km/population_conus_total_ssp5_2020_2100_wrf_wgs84.csv"
reference_temp_F_i = 65
folder_i="C:/Z/projects/current/00_IM3/tests/process_hdhcdh/outputs"
diagnostics_i = T
xml_i = T
name_append_i = "_ssp5pop"

hdcd(ncdf = ncdf_i,
     spatial = spatial_i,
     temporal = temporal_i,
     population = population_i,
     reference_temp_F = reference_temp_F_i,
     folder = folder_i,
     diagnostics= diagnostics_i,
     xml= xml_i,
     name_append = name_append_i,
     save = F) -> test_hdcd


ncdf = ncdf_i
spatial = spatial_i
temporal = temporal_i
population = population_i
reference_temp_F = reference_temp_F_i
folder = folder_i
diagnostics= diagnostics_i
xml= xml_i
name_append = name_append_i
