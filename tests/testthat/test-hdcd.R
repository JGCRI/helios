context("hdcd")
library(helios); library(testthat); library(dplyr)


#.......................
# Prepare data for tests
#.......................

# path to climate data example
wrf_usa_ncdf <- system.file(
  'extras',
  'wrfout_d01_2020-01-01_01%3A00%3A00_sub.nc',
  package = 'helios')
cmip6_china_ncdf <- system.file(
  'extras',
  'gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020_sub.nc',
  package = 'helios')

# path to population data example
pop_usa_csv <- system.file(
  'extras',
  'population_conus_ssp2_2020wrf_wgs84.csv',
  package = 'helios')
pop_china_ncdf <- system.file(
  'extras',
  'ssp1_2020_sub.nc',
  package = 'helios')

hdcd_usa <- helios::hdcd(ncdf = wrf_usa_ncdf,
                         ncdf_var = 'T2',
                         model = 'wrf',
                         population = pop_usa_csv,
                         spatial = 'states_us_49',
                         temporal = 2020,
                         reference_temp_F = 65,
                         diagnostics = F,
                         xml = F,
                         name_append = "",
                         save = F)

hdcd_china <- helios::hdcd(ncdf = cmip6_china_ncdf,
                           ncdf_var = 'tas',
                           model = 'cmip',
                           population = pop_china_ncdf,
                           spatial = 'gcam_region_32',
                           temporal = 2020,
                           reference_temp_F = 65,
                           diagnostics = F,
                           xml = F,
                           name_append = "",
                           save = F)

testthat::skip_on_cran(); testthat::skip_on_travis()

# ------------------------------------
# Testing Outputs
# ------------------------------------
test_that("returns a list containing 3 elements", {
  testthat::expect_equal(length(hdcd_usa), 3)
  testthat::expect_equal(length(hdcd_china), 3)
})

test_that("segment, monthly, and annual output exists", {
  testthat::expect_gt(nrow(hdcd_usa$hdcd_comb), 0)
  testthat::expect_gt(nrow(hdcd_usa$hdcd_comb_monthly), 0)
  testthat::expect_gt(nrow(hdcd_usa$hdcd_comb_annual), 0)

  testthat::expect_gt(nrow(hdcd_china$hdcd_comb), 0)
  testthat::expect_gt(nrow(hdcd_china$hdcd_comb_monthly), 0)
  testthat::expect_gt(nrow(hdcd_china$hdcd_comb_annual), 0)
})

test_that("monthly and annual HDD values are negative, CDD values are positive", {

  testthat::expect_equal(all(hdcd_usa$hdcd_comb_monthly$value[hdcd_usa$hdcd_comb_monthly$HDDCDD == 'HDD'] <= 0), TRUE)
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_monthly$value[hdcd_usa$hdcd_comb_monthly$HDDCDD == 'CDD'] >= 0), TRUE)
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_annual$value[hdcd_usa$hdcd_comb_annual$HDDCDD == 'HDD'] <= 0), TRUE)
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_annual$value[hdcd_usa$hdcd_comb_annual$HDDCDD == 'CDD'] >= 0), TRUE)
})

# ------------------------------------
# Testing Errors
# ------------------------------------

# To do: Get smaller example to test
# hdcd(ncdf = "example_ncdf_wrfout_d01_1979-01-01_00_00_00.nc",
#      spatial = "gcamusa",
#      temporal = NULL,
#      population = NULL,
#      reference_temp_F = 65,
#      #folder = folder_i,
#      diagnostics= F,
#      xml= F,
#      name_append = "",
#      save = F) -> test_hdcd

# Test outputs hdcd_comb
# test_that("hdcd_comb works", {
#   #testthat::expect_true(nrow(test_hdcd$hdcd_comb)>1)
#   testthat::expect_true(1==1)
# })


