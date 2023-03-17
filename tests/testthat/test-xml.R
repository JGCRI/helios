testthat::skip_on_cran()
testthat::skip_on_travis()
testthat::skip_on_ci()

test_that("save xml produces xml outputs", {
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'hdcd_wrf_to_gcam_2020_2020.xml'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'hdcd_wrf_to_gcam_2020_2020_xml.csv'))
})
