context("Box-Cox Transforming Numeric Vector")

test_that("bctrans works on some particular values.", {
    expect_identical(bctrans(c(1, 1, 1), 0, FALSE), c(0, 0, 0))
    expect_identical(bctrans(c(1, 1, 1), 2, FALSE), c(0, 0, 0))
    expect_identical(bctrans(c(2, 2, 2), 2, FALSE), c(3/2, 3/2, 3/2))
})


