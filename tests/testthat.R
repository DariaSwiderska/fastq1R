library(testthat)
library(fastq1R)

test_check("fastq1R")

test_that("Parameter", {
  expect_error(fastq1R(!is.character(seq)))
})
