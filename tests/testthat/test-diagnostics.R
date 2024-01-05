

test_that('dianostics produces outputs', {

  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_ci()

  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'segment_2020-2025.csv'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'segment_2020.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'segment_2025.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'segment_allYears_fixedScale.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'segment_allYears_freeScale.png'))

  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_2020-2025_noaa-2000-2021.csv'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_2020_noaa-2020.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_2025_noaa-2021.png'))
  testthat::expect_snapshot_file(
    testthat::test_path('output', 'diagnostics', 'monthly_allYears_noaa-2021.png'))

})


# ------------------------------------
# Testing Error Messages
# ------------------------------------

test_that('warning message when conditions not apply', {

  testthat::expect_message(run_diagnostics(hdcd_segment = helios::example_hdcd_monthly_usa))

  testthat::expect_message(run_diagnostics(hdcd_segment = tibble::tibble()))

  testthat::expect_message(run_diagnostics(hdcd_monthly = tibble::tibble()))

  testthat::expect_message(
    run_diagnostics(hdcd_segment = helios::example_hdcd_segment_usa %>%
                      dplyr::filter(year %in% 2020, segment == 'Jan_day')))
  testthat::expect_message(
    run_diagnostics(hdcd_monthly = helios::example_hdcd_monthly_usa %>%
                      dplyr::filter(year %in% 2020, month == 1)))

})
