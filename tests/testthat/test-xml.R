testthat::skip_on_cran()
testthat::skip_on_travis()
testthat::skip_on_ci()

test_that("save xml produces xml outputs", {
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'hdcd_wrf_2020_2020_gcam.xml'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'hdcd_wrf_2020_2020_gcam_xml.csv'))
})
