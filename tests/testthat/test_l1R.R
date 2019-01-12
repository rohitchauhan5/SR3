context('l1R tests')

x <- matrix(1, 10, 1)
checkmate::expect_double(l1R(x), lower = 0)
