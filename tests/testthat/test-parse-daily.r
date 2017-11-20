context("parse-daily")

test_that("current season parser works", {
  # nba data
  j <- read_msf("nba-current-season.json")

  actual <- parse_current_season(j)

  expect_equal(nrow(actual), 1L)
  expect_equal(ncol(actual), 7L)
  expect_equal(nrow(actual$supportedPlayerStats[[1]]), 64)
  expect_equal(ncol(actual$supportedPlayerStats[[1]]), 3)
  expect_equal(nrow(actual$supportedTeamStats[[1]]), 67)
  expect_equal(ncol(actual$supportedTeamStats[[1]]), 3)
})

test_that("daily dfs parser works", {
  # nhl data
  j <- read_msf("nhl-daily-dfs.json")

  actual <- parse_daily_dfs(j, "draftkings")

  expect_equal(nrow(actual), 413L)
  expect_equal(ncol(actual), 9L)
})

test_that("daily game schedule parser works", {
  # nfl data
  j <- read_msf("nfl-daily-game-schedule.json")

  actual <- parse_daily_game_schedule(j)

  expect_equal(nrow(actual), 1L)
  expect_equal(ncol(actual), 7L)
})

test_that("daily player stats parser works", {
  # nba data
  j <- read_msf("nba-daily-player-stats.json")

  actual <- parse_daily_player_stats(j)

  expect_equal(nrow(actual), 1L)
  expect_equal(ncol(actual), 68L)
})
