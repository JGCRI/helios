library(helios); library(dplyr); library(raster)

#................
# Initialize
#...............

ncdf_i = c("C:/Z/projects/current/00_IM3/tests/hdhcdh/wrfout_d01_1979-01-01_00%3A00%3A00",
           "C:/Z/projects/current/00_IM3/tests/hdhcdh/wrfout_d01_1994-12-17_01%3A00%3A00.nc")
spatial_i = "gcamusa"
temporal_i = NULL # "gcamusa"
population_i = NULL
reference_temp_F_i = 65
folder_i=NULL
diagnostics_i = T

hdcd(ncdf = ncdf_i,
     spatial = spatial_i,
     temporal = temporal_i,
     population = population_i,
     reference_temp_F = reference_temp_F_i,
     diagnostics= diagnostics_i)


ncdf = ncdf_i
spatial = spatial_i
temporal = temporal_i
population = population_i
reference_temp_F = reference_temp_F_i
