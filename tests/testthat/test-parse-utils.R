context("parse-utils")

test_that("transpose and simplify works", {
  x <- list(
    list(a = 1, b = 2, d = 3),
    list(a = 10, d = 30, f = 40)
    )

  actual <- transpose_and_simplify(x)
  expected <- list(
    a = c(1, 10), b = c(2, NA), d = c(3, 30), f = c(NA, 40)
  )

  expect_equal(actual, expected)
})
