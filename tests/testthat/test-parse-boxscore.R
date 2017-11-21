context("parse-boxscore")

test_that("nhl boxscore parser works", {
  # nhl data
  j <- read_msf("nhl-boxscore.json")

  actual <- parse_boxscore(j)
  col_types <- vapply(actual[4:38], class, FUN.VALUE = character(1L))

  expect_equal(nrow(actual), 39L)
  expect_equal(ncol(actual), 38L)
  expect_true(all(col_types == "numeric"))
})


test_that("mlb boxscore parser works", {
  # mlb data
  j <- read_msf("mlb-boxscore.json")

  actual <- parse_boxscore(j)
  col_types <- vapply(actual[4:135], class, FUN.VALUE = character(1L))

  expect_equal(nrow(actual), 30L)
  expect_equal(ncol(actual), 135L)
  expect_true(all(col_types == "numeric"))
})

test_that("nfl boxscore parser works", {
  # nfl data
  j <- read_msf("nfl-boxscore.json")

  actual <- parse_boxscore(j)
  col_types <- vapply(actual[4:139], class, FUN.VALUE = character(1L))

  expect_equal(nrow(actual), 87L)
  expect_equal(ncol(actual), 139L)
  expect_true(all(col_types == "numeric"))
})

test_that("nfl boxscore parser skips players when they have NULL stats", {
  # nfl data
  j <- read_msf("nfl-boxscore-with-null-stats.json")

  actual <- parse_boxscore(j)
  col_types <- vapply(actual[4:139], class, FUN.VALUE = character(1L))

  expect_equal(nrow(actual), 88L) # originally had 89
  expect_equal(ncol(actual), 139L)
  expect_true(all(col_types == "numeric"))
})
