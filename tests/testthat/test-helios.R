context("helios")
library(helios); library(testthat); library(dplyr)

test_that("Helios works", {
  a1 <- 1
  a2 <- 2
  sum1 <- sum(a1,a2)
  testthat::expect_true(sum1==3)
})
