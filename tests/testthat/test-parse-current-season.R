context("parse-current-season")

test_that("current season parser works", {
  # nhl data
  j <- read_msf("nba-current-season.json")

  actual <- parse_current_season(j)

  expect_equal(nrow(actual), 1L)
  expect_equal(ncol(actual), 7L)
  expect_equal(nrow(actual$supportedPlayerStats[[1]]), 64)
  expect_equal(ncol(actual$supportedPlayerStats[[1]]), 3)
  expect_equal(nrow(actual$supportedTeamStats[[1]]), 67)
  expect_equal(ncol(actual$supportedTeamStats[[1]]), 3)
})
