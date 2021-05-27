test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Parameter", {
  expect_error(fastq1R(!is.character(seq)))
})
