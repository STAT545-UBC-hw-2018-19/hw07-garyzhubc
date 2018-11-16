context("Inverse Box-Cox Transforming Numeric Vector")

test_that("bctrans_inv works on some particular values.", {
    expect_identical(bctrans_inv(c(0, 0, 0), 0, FALSE), c(1, 1, 1))
    expect_identical(bctrans_inv(c(0, 0, 0), 2, FALSE), c(1, 1, 1))
    expect_identical(bctrans_inv(c(3/2, 3/2, 3/2), 2, FALSE), c(2, 2, 2))
})
