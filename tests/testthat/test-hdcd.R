
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
# Testing Diagnostics and XML
# ------------------------------------
run_hdcd_usa(diagnostics = T, xml = T)


# ------------------------------------
# Testing Conditions
# ------------------------------------
test_that('if temporal is NULL, set as 2020 - 2100', {
  testthat::expect_message(
    run_hdcd_usa(temporal = NULL),
    'Setting time periods to default 2020 to 2100 with 5 year interval.')
})

# ------------------------------------
# Testing Error Messages
# ------------------------------------
test_that('wrong climate variable name gives error', {
  testthat::expect_error(run_hdcd_usa(ncdf_var = 'var'), class = 'error')
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

test_that('invalid spatial input type gives error', {
  testthat::expect_error(run_hdcd_usa(spatial = 'gcam'), class = 'error')
})

test_that('invalid temporal input type gives error', {
  testthat::expect_error(run_hdcd_usa(temporal = '2020'), class = 'error')
})


# ------------------------------------
# Testing Package Example Helper
# ------------------------------------
test_that('pkg_example gives paths', {
  testthat::expect_type(helios::pkg_example(), 'character')
  testthat::expect_type(helios::pkg_example('ssp1_2020_sub.nc'), 'character')
})
