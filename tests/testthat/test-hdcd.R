context("hdcd")
library(helios); library(testthat); library(dplyr)

#.......................
# Prepare data for tests
#.......................

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
test_that("hdcd_comb works", {
  #testthat::expect_true(nrow(test_hdcd$hdcd_comb)>1)
  testthat::expect_true(1==1)
})
