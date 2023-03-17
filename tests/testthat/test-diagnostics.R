testthat::skip_on_cran()
testthat::skip_on_travis()
testthat::skip_on_ci()


test_that("dianostics produces outputs", {
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'hdcd_wrf_to_gcam_2020_2020_2020-2020.csv'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_ncdf_2020-2020_noaa_2000-2021.csv'))

  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'hdcd_wrf_to_gcam_2020_2020_2020.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'hdcd_wrf_to_gcam_2020_2020_allYears_gradient_fixedScale_.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'hdcd_wrf_to_gcam_2020_2020_allYears_gradient_freeScale_.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_ncdf_2020_noaa_2020.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_ncdf_AllYears_noaa_2021.png'))
})
