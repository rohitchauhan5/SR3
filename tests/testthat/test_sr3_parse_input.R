context('input parsing')

test_that("parser returns a list of 18 elements", {
  A <- matrix(0, 3, 4)
  b <- matrix(0, 3, 1)
  m <- dim(A)[1]
  n <- dim(A)[2]

  p_length <- length(sr3_parse_input(A, b, m, n))
  expect_equal(p_length, 18)
})

test_that("tall matrix A returns correct value", {
  A <-
    matrix(
      c(-8,     7,     6,     1,     3,
       10 ,    3 ,   -2 ,   -4 ,   -3,
       -10,    -3,    -5,     5,     7,
       6  ,   0  ,  -2  ,  -7  ,   1,
       7  ,  -2  ,  -8  ,   4  ,  -3,
       8  ,  -9  ,  -8  ,  -7  ,   9,
       -9 ,   -5 ,    9 ,   -3 ,    8,
       -2 ,   -8 ,   10 ,    3 ,    1,
       -5 ,   -7 ,    2 ,    6 ,    3,
       6  ,  -5  ,  -9  ,  -9  ,   2,
       -1 ,   -2 ,   -6 ,    9 ,   -6,
       9  ,  -9  ,  -3  ,   6  ,  -4,
       -7 ,    8 ,    7 ,    0 ,   -1,
       -5 ,    9 ,  -10 ,   -1 ,   -6,
       -7 ,    0 ,  -10 ,   -1 ,    7,
       -8 ,    0 ,   -7 ,   -4 ,   -6,
       8  ,  -3  ,   3  ,   0  ,  -6,
       2  ,   8  ,   5  ,   0  ,  -7,
       1  ,  -3  ,   3  ,   7  ,  -6,
       -7 ,   -8 ,   -1 ,    6 ,   -1),
      20, 5, byrow=TRUE)
 
  b <-
  matrix(
    c(1, -1, -8, -5, -2, 2, -5, 2, 4, -6, -8,
     -4, -4, -2, 0, -9, -5, 6, -10, 9),
    20, 1, byrow=TRUE)
 
  m <- dim(A)[1]
  n <- dim(A)[2]
  
  result <- sr3(A = A, b = b)
  x <- result$x
  w <- result$w
  print(w)
  result_from_MATLAB_x <- matrix(c(0.1993, 0.2393, 0.2005, 0.2122, 0.4120), 5, 1, byrow=TRUE)
  result_from_MATLAB_w <- matrix(0, 5, 1, byrow=TRUE)
  expect_equal(x, result_from_MATLAB_x, tolerance=1e-4)
  expect_equal(w, result_from_MATLAB_w, tolerance=1e-4)
})



#   A <-
#      matrix(
#        c(-8     7     6     1     3;
#         10     3    -2    -4    -3;
#         -10    -3    -5     5     7;
#         6     0    -2    -7     1;
#         7    -2    -8     4    -3;
#         8    -9    -8    -7     9;
#         -9    -5     9    -3     8;
#         -2    -8    10     3     1;
#         -5    -7     2     6     3;
#         6    -5    -9    -9     2;
#         -1    -2    -6     9    -6;
#         9    -9    -3     6    -4;
#         -7     8     7     0    -1;
#         -5     9   -10    -1    -6;
#         -7     0   -10    -1     7;
#         -8     0    -7    -4    -6;
#         8    -3     3     0    -6;
#         2     8     5     0    -7;
#         1    -3     3     7    -6;
#         -7    -8    -1     6    -1,;
#       20, 5, byrow=TRUE)
#  
#   b <-
#   matrix(
#     c(1, -1, -8, -5, -2, 2, -5, 2, 4, -6, -8,
#      -4, -4, -2, 0, -9, -5, 6, -10, 9),
#     20, 1, byrow=TRUE)