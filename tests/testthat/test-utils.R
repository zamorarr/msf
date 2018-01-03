context("utils")

test_that("camel to underscore works", {
  x <- c("heyIAmCool", "HelloGoodbye")
  actual <- camel_to_underscore(x)
  expected <- c("hey_i_am_cool", "hello_goodbye")
  expect_equal(actual, expected)
})
