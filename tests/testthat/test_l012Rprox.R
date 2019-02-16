# #
# test_that("tall matrix A returns correct value", {
#   A <-
#     matrix(
#       c(-8,     7,     6,     1,     3,
#         10 ,    3 ,   -2 ,   -4 ,   -3,
#         -10,    -3,    -5,     5,     7,
#         6  ,   0  ,  -2  ,  -7  ,   1,
#         7  ,  -2  ,  -8  ,   4  ,  -3,
#         8  ,  -9  ,  -8  ,  -7  ,   9,
#         -9 ,   -5 ,    9 ,   -3 ,    8,
#         -2 ,   -8 ,   10 ,    3 ,    1,
#         -5 ,   -7 ,    2 ,    6 ,    3,
#         6  ,  -5  ,  -9  ,  -9  ,   2,
#         -1 ,   -2 ,   -6 ,    9 ,   -6,
#         9  ,  -9  ,  -3  ,   6  ,  -4,
#         -7 ,    8 ,    7 ,    0 ,   -1,
#         -5 ,    9 ,  -10 ,   -1 ,   -6,
#         -7 ,    0 ,  -10 ,   -1 ,    7,
#         -8 ,    0 ,   -7 ,   -4 ,   -6,
#         8  ,  -3  ,   3  ,   0  ,  -6,
#         2  ,   8  ,   5  ,   0  ,  -7,
#         1  ,  -3  ,   3  ,   7  ,  -6,
#         -7 ,   -8 ,   -1 ,    6 ,   -1),
#       20, 5, byrow=TRUE)
#   
#   b <-
#     matrix(
#       c(1, -1, -8, -5, -2, 2, -5, 2, 4, -6, -8,
#         -4, -4, -2, 0, -9, -5, 6, -10, 9),
#       20, 1, byrow=TRUE)
#   
#   m <- dim(A)[1]
#   n <- dim(A)[2]
#   
#   x <- sr3(A, b)
#   result_from_MATLAB <- matrix(c(0.1993, 0.2393, 0.2005, 0.2122, 0.4120), 5, 1, byrow=TRUE)
#   expect_equal(x, result_from_MATLAB, tolerance=1e-4)
# })

test_that("Checking the value of mode == 1, ifprox == TRUE",{
  x_test <- c(-171,
         336,
         -51,
         123,
         -168,
         182,
         -90,
         223,
         252,
         299)
  result_mode_1_ifprox_TRUE <- l012Rprox(x = x_test, alpha = 10000,l0w = 1,l1w = 0,l2w = 0,ifprox = TRUE)
  result_from_MATLAB <- c(-171,
                          336,
                          0,
                          0,
                          -168,
                          182,
                          0,
                          223,
                          252,
                          299)
  expect_equal(result_mode_1_ifprox_TRUE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 1, ifprox == FALSE",{
  x_test <- c(12,0,324,-234,0,0,2354,0,0,-7)
  result_mode_1_ifprox_FALSE <- l012Rprox(x = x_test, alpha = 10000,l0w = 1,l1w = 0,l2w = 0,ifprox = FALSE)
  result_from_MATLAB <- 5
  expect_equal(result_mode_1_ifprox_FALSE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 2, ifprox == TRUE",{
  x_test <- c(-1377,
              -1096,
              -587,
              -1309,
              533,
              -1416,
              -1323,
              -1488,
              -1317,
              -693,
              -1067,
              771,
              -709,
              -1446,
              715,
              940,
              -683,
              -1667,
              -1226,
              -774,
              -215,
              -1214,
              -191,
              134,
              -1335,
              -1648,
              -1110,
              -1044,
              -728,
              -476)
  result_mode_2_ifprox_TRUE <- l012Rprox(x = x_test, alpha = 1000,l0w = 0,l1w = 1,l2w = 0,ifprox = TRUE)
  result_from_MATLAB <- c(-377,
                          -96,
                          0,
                          -309,
                          0,
                          -416,
                          -323,
                          -488,
                          -317,
                          0,
                          -67,
                          0,
                          0,
                          -446,
                          0,
                          0,
                          0,
                          -667,
                          -226,
                          0,
                          0,
                          -214,
                          0,
                          0,
                          -335,
                          -648,
                          -110,
                          -44,
                          0,
                          0
  )
  expect_equal(result_mode_2_ifprox_TRUE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 2, ifprox == FALSE",{
  x_test <- c(   172,
                 168,
                 0,
                 110,
                 -41,
                 -91,
                 123,
                 50,
                 44,
                 172,
                 83,
                 85,
                 158,
                 142,
                 73,
                 -45,
                 -28,
                 166,
                 -92,
                 47,
                 -50,
                 194,
                 114,
                 50,
                 41,
                 -83,
                 105,
                 -88,
                 -79,
                 57)
  result_mode_2_ifprox_FALSE <- l012Rprox(x = x_test, alpha = 1000,l0w = 0,l1w = 1,l2w = 0,ifprox = FALSE)
  result_from_MATLAB <- 2751
  expect_equal(result_mode_2_ifprox_FALSE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 3, ifprox == TRUE",{
  x_test <- c(   471,
                 137,
                 117,
                 -115,
                 91,
                 199,
                 243,
                 16,
                 -6,
                 491,
                 -270,
                 409,
                 431,
                 337,
                 -221,
                 -91,
                 -32,
                 244,
                 -191,
                 277,
                 -215,
                 223,
                 95,
                 324,
                 272)
  result_mode_3_ifprox_TRUE <- l012Rprox(x = x_test, alpha = 100,l0w = 1,l1w = 1,l2w = 0,ifprox = TRUE)
  result_from_MATLAB <- c(       371,
                                 37,
                                 17,
                                 -15,
                                 0,
                                 99,
                                 143,
                                 0,
                                 0,
                                 391,
                                 -170,
                                 309,
                                 331,
                                 237,
                                 -121,
                                 0,
                                 0,
                                 144,
                                 -91,
                                 177,
                                 -115,
                                 123,
                                 0,
                                 224,
                                 172)
  expect_equal(result_mode_3_ifprox_TRUE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 3, ifprox == FALSE",{
   x_test <-c(-1353,
              -256,
              -257,
              -402,
              -1273,
              -497,
              -712,
              -21,
              -513,
              -283,
              -810,
              -843,
              -245,
              -1374,
              -1298,
              -1237,
              -906,
              -236,
              -279,
              -1409,
              -893,
              -699,
              -867,
              -501,
              -545,
              -1056,
              -844,
              -1477,
              -4,
              -1246,
              -1339,
              -934,
              -1199,
              -756,
              -984,
              -53,
              -101,
              -1420,
              -378,
              -1091)
   result_mode_3_ifprox_FALSE <- l012Rprox(x = x_test, alpha = 1000,l0w = 1,l1w = 1,l2w = 0,ifprox = FALSE)
   result_from_MATLAB <- 30631
   expect_equal(result_mode_3_ifprox_FALSE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 4, ifprox == TRUE",{
  x_test <- c(           -20,
                         418,
                         1800,
                         -38,
                         1941,
                         -445,
                         954,
                         832,
                         387,
                         944,
                         833,
                         -877,
                         -1052,
                         1997,
                         -901,
                         -1386,
                         464,
                         1587,
                         842,
                         -834,
                         -209,
                         113,
                         1936,
                         -953,
                         1495,
                         757,
                         -183,
                         -832,
                         -1,
                         187,
                         -1078,
                         563,
                         -709,
                         -154,
                         541)
  result_mode_4_ifprox_TRUE <- l012Rprox(x = x_test, alpha = 1000,l0w = 0,l1w = 0,l2w = 1,ifprox = TRUE)
  result_from_MATLAB <- c(   -0.0200,
                             0.4176,
                             1.7982,
                             -0.0380,
                             1.9391,
                             -0.4446,
                             0.9530,
                             0.8312,
                             0.3866,
                             0.9431,
                             0.8322,
                             -0.8761,
                             -1.0509,
                             1.9950,
                             -0.9001,
                             -1.3846,
                             0.4635,
                             1.5854,
                             0.8412,
                             -0.8332,
                             -0.2088,
                             0.1129,
                             1.9341,
                             -0.9520,
                             1.4935,
                             0.7562,
                             -0.1828,
                             -0.8312,
                             -0.0010,
                             0.1868,
                             -1.0769,
                             0.5624,
                             -0.7083,
                             -0.1538,
                             0.5405)
  expect_equal(result_mode_4_ifprox_TRUE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 4, ifprox == FALSE",{
  x_test <- c(       -1367,
                     -1193,
                     277,
                     -1306,
                     1210,
                     1922,
                     786,
                     -953,
                     128,
                     -2015,
                     1579,
                     1459,
                     1180,
                     -1327,
                     175,
                     -2399,
                     -586,
                     -1093,
                     -1774,
                     -1696)
  result_mode_4_ifprox_FALSE <- l012Rprox(x = x_test, alpha = 1000,l0w = 0,l1w = 0,l2w = 1,ifprox = FALSE)
  result_from_MATLAB <- 184788575
  expect_equal(result_mode_4_ifprox_FALSE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 5, ifprox == TRUE",{
  x_test<-c(     -1116,
                 -2892,
                 -168,
                 -857,
                 358,
                 380,
                 48,
                 -3219,
                 -3029,
                 -1674,
                 -533,
                 134,
                 -1199,
                 1028,
                 479,
                 1831,
                 -531,
                 -1644,
                 -2830,
                 -101,
                 806,
                 -1113,
                 -2910,
                 -1961,
                 -2571,
                 -1883,
                 -1024,
                 -553,
                 -930,
                 1327,
                 -603,
                 1696,
                 44,
                 1772,
                 -2100,
                 251,
                 -1839,
                 228,
                 354,
                 -3033,
                 -2024,
                 -2190,
                 206,
                 1160,
                 -1540)
  result_mode_5_ifprox_TRUE <- l012Rprox(x = x_test, alpha = 1000,l0w = 1,l1w = 0,l2w = 1,ifprox = TRUE)
  result_from_MATLAB <- c(                     0,
                                               -2.889110889110889,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               -3.215784215784216,
                                               -3.025974025974026,
                                               -1.672327672327672,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               1.829170829170829,
                                               0,
                                               -1.642357642357642,
                                               -2.827172827172827,
                                               0,
                                               0,
                                               0,
                                               -2.907092907092907,
                                               -1.959040959040959,
                                               -2.568431568431568,
                                               -1.881118881118881,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               1.694305694305694,
                                               0,
                                               1.770229770229770,
                                               -2.097902097902098,
                                               0,
                                               -1.837162837162837,
                                               0,
                                               0,
                                               -3.029970029970030,
                                               -2.021978021978022,
                                               -2.187812187812188,
                                               0,
                                               0,
                                               -1.538461538461539)
  expect_equal(result_mode_5_ifprox_TRUE, result_from_MATLAB, tolerance=1e-4)
})

test_that("Checking the value of mode == 5, ifprox == FALSE",{
  x_test <- c(   91,
                 38,
                 -297,
                 1,
                 -107,
                 158,
                 -300,
                 -69,
                 -88,
                 -70,
                 85,
                 -139,
                 93,
                 -64,
                 -283)
  result_mode_5_ifprox_FALSE <- l012Rprox(x = x_test, alpha = 10000,l0w = 1,l1w = 0,l2w = 1,ifprox = FALSE)
  result_from_MATLAB <- 180581.5
  expect_equal(result_mode_5_ifprox_FALSE, result_from_MATLAB, tolerance=1e-4)
})

# test_that("Checking the value of mode == 6, ifprox == TRUE",{
# 
# })

# test_that("Checking the value of mode == 6, ifprox == FALSE",{
#   
# })
# 
# test_that("Checking the value of mode == 7, ifprox == TRUE",{
#   
# })
# 
# test_that("Checking the value of mode == 7, ifprox == FALSE",{
#   
# })


