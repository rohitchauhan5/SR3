context('input parsing')

test_that("parser returns a list of 18 elements", {
  A <- matrix(0, 3, 4)
  b <- matrix(4, 1)
  m <- dim(A)[1]
  n <- dim(A)[2]

  expect_equal(length(sr3_parse_input(A, b, m, n)), 18)
})
