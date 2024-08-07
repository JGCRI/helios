
testthat::skip_on_cran()
testthat::skip_on_travis()

# ------------------------------------
# Testing Outputs from Major Functions
# ------------------------------------

hdcd_usa <- run_hdcd_usa()
hdcd_china <- run_hdcd_china()

test_that("returns a list containing 3 elements", {
  testthat::expect_equal(length(hdcd_usa), 3)
  testthat::expect_equal(length(hdcd_china), 3)
})

test_that("segment, monthly, and annual output exists", {
  testthat::expect_gt(nrow(hdcd_usa$hdcd_comb_gcam), 0)
  testthat::expect_gt(nrow(hdcd_usa$hdcd_comb_monthly), 0)
  testthat::expect_gt(nrow(hdcd_usa$hdcd_comb_annual), 0)

  testthat::expect_gt(nrow(hdcd_china$hdcd_comb_gcam), 0)
  testthat::expect_gt(nrow(hdcd_china$hdcd_comb_monthly), 0)
  testthat::expect_gt(nrow(hdcd_china$hdcd_comb_annual), 0)
})

test_that("monthly and annual HD values are negative, CD values are positive", {
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_monthly$value[hdcd_usa$hdcd_comb_monthly$HDCD == 'HD'] <= 0), TRUE)
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_monthly$value[hdcd_usa$hdcd_comb_monthly$HDCD == 'CD'] >= 0), TRUE)
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_annual$value[hdcd_usa$hdcd_comb_annual$HDCD == 'HD'] <= 0), TRUE)
  testthat::expect_equal(all(hdcd_usa$hdcd_comb_annual$value[hdcd_usa$hdcd_comb_annual$HDCD == 'CD'] >= 0), TRUE)
})

# ------------------------------------
# Testing Diagnostics and XML
# ------------------------------------

run_diagnostics()
run_hdcd_usa(xml = TRUE)

# ------------------------------------
# Testing Different scales
# ------------------------------------

run_hdcd_china(spatial = 'gcam_countries')
run_hdcd_china(spatial = data.frame(subRegion = 'China'))

# ------------------------------------
# Testing Conditions
# ------------------------------------
test_that('if time_periods is NULL, set as 2020 - 2100', {
  testthat::expect_message(run_hdcd_usa(time_periods = NULL))
})

# ------------------------------------
# Testing Error Messages
# ------------------------------------
test_that('wrong climate variable name gives error', {
  testthat::expect_error(run_hdcd_usa(ncdf_var = 'var'), class = 'error')
})

test_that('wrong climate variable name gives error', {
  testthat::expect_error(run_hdcd_china(ncdf_var = 'var'), class = 'error')
})


test_that('wrong climate model name gives error', {
  testthat::expect_error(run_hdcd_usa(model = 'model'), class = 'error')
})

test_that('fail to provide model name gives error', {
  testthat::expect_error(run_hdcd_usa(model = NULL), class = 'error')
})

test_that('No population input format gives error', {
  testthat::expect_error(run_hdcd_usa(population = NULL), class = 'error')
})

test_that('invalid population input type gives error', {
  testthat::expect_error(run_hdcd_usa(population = pop_test), class = 'error')
})

test_that('population input that does not exist gives error', {
  testthat::expect_error(run_hdcd_usa(population = 'invalid/path/population.csv'), class = 'error')
})

test_that('different resolution between population and climate data gives error', {
  testthat::expect_error(run_hdcd_china(population = helios::pkg_example('population_conus_ssp2_2020wrf_wgs84.csv')), class = 'error')
})

test_that('invalid spatial input type gives error', {
  testthat::expect_error(run_hdcd_usa(spatial = 'gcam'), class = 'error')
})

test_that('invalid temporal input type gives error', {
  testthat::expect_error(run_hdcd_usa(time_periods = '2020'), class = 'error')
})

test_that('set spatial arg to other than gcam_us49 when dispatch_segment = T gives error', {
  testthat::expect_error(run_hdcd_usa(spatial = 'gcam_regions32'), class = 'error')
})

test_that('invalid dispatch segment when spatial is not right gives error', {
  testthat::expect_error(run_hdcd_china(dispatch_segment = T), class = 'error')
})

test_that('invalid model_timestep input gives error', {
  testthat::expect_error(run_hdcd_china(model_timestep = 'annual'), class = 'error')
})





# ------------------------------------
# Testing Package Example Helper
# ------------------------------------
test_that('pkg_example gives paths', {
  testthat::expect_type(helios::pkg_example(), 'character')
  testthat::expect_type(helios::pkg_example('ssp1_2020_sub.nc'), 'character')
})
