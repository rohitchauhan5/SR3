context('input parsing')

test_that("parser returns a list of 18 elements", {
  # Input validation for default arugments should
  # work if this test passes.
  A <- matrix(0, 3, 4)
  b <- matrix(4, 1)
  m <- dim(A)[1]
  n <- dim(A)[2]

  p_length <- length(sr3_parse_input(A, b, m, n))
  expect_equal(p_length, 18)
})
