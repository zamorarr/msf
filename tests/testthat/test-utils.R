context("utils")

test_that("multiplication works", {
  x <- c("heyIAmCool", "HelloGoodbye")
  actual <- camel_to_underscore(x)
  expected <- c("hey_i_am_cool", "hello_goodbye")
  expect_equal(2 * 2, 4)
})
