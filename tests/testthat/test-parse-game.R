context("parse-game")

test_that("nba starting lineup parser works", {
    # nba data
    resp <- with_mock_test(game_starting_lineup("nba", "20171029-DEN-BRO", season = "2017-2018-regular"))

    actual <- parse_starting_lineup(resp$content)

    expect_equal(nrow(actual), 26L)
    expect_equal(ncol(actual), 4L)
    expect_equal(actual$player_id[1:3], c("9188", "9093", "10134"))
    expect_equal(actual$team_id[1:3], rep("99", 3L))
    expect_equal(actual$game_id[1:3], rep("42157", 3L))
    expect_equal(actual$lineup_position[1:3], c("Starter1", "Starter2", "Starter5"))
})

test_that("mlb starting lineup parser works", {
  # mlb data
  resp <- with_mock_test(resp <- game_starting_lineup("mlb", "20171005-BOS-HOU", season = "latest"))

  actual <- parse_starting_lineup(resp$content)

  expect_equal(nrow(actual), 40L)
  expect_equal(ncol(actual), 4L)
  expect_equal(actual$player_id[1:3], c("12551", "11061", "11339"))
  expect_equal(actual$team_id[1:3], rep("113", 3L))
  expect_equal(actual$game_id[1:3], rep("43316", 3L))
  expect_equal(actual$lineup_position[1:3], c("BO7", "BO8", "BO5"))
})

test_that("nhl play by play lineup parser works", {
    # nhl data
    resp <- with_mock_test(game_pbp("nhl", "20161215-FLO-WPJ", season = "2016-2017-regular"))

    actual <- parse_game_pbp(resp$content, "nhl")

    expect_equal(nrow(actual), 187L)
    expect_equal(ncol(actual), 4L)
    expect_equal(actual$period[1:3], rep("1", 3L))
    expect_equal(actual$time[1:3], c("0:00", "0:07", "0:41"))
    expect_equal(actual$event[1:3], c("faceoff", "shot", "hit"))
})

test_that("mlb play by play lineup parser works", {
    # mlb data
    resp <- with_mock_test(game_pbp("mlb", "20170523-LAA-TB", season = "2017-regular"))

    actual <- parse_game_pbp(resp$content, "mlb")

    expect_equal(nrow(actual), 70L)
    expect_equal(ncol(actual), 5L)
    expect_equal(actual$inning[1:3], rep("1", 3L))
    expect_equal(actual$inning_half[1:3], rep("top", 3L))
    expect_equal(actual$batting_team[1:3], rep("124", 3L))
    expect_equal(actual$atbat_id[1:3], 1L:3L)
})
