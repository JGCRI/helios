context("hdcd")
library(helios); library(testthat); library(dplyr)

test_that("hdcd works", {

  testthat::expect_true(helios::hdcd("a","b")==1)
})
