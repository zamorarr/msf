context("parse-game")

with_mock_API({
  test_that("nba starting lineup parser works", {
    # nba data
    resp <- game_starting_lineup("nba", "20171029-DEN-BRO", season = "2017-2018-regular")

    actual <- parse_starting_lineup(resp$content)

    expect_equal(nrow(actual), 26L)
    expect_equal(ncol(actual), 4L)
    expect_equal(actual$player_id[1:3], c("9188", "9093", "10134"))
    expect_equal(actual$team_id[1:3], rep("99", 3L))
    expect_equal(actual$game_id[1:3], rep("42157", 3L))
    expect_equal(actual$lineup_position[1:3], c("Starter1", "Starter2", "Starter5"))
  })
})

with_mock_API({
  test_that("nhl play by play lineup parser works", {
    # nhl data
    resp <- game_pbp("nhl", "20161215-FLO-WPJ", season = "2016-2017-regular")

    actual <- parse_game_pbp(resp$content, "nhl")

    expect_equal(nrow(actual), 187L)
    expect_equal(ncol(actual), 4L)
    expect_equal(actual$period[1:3], rep("1", 3L))
    expect_equal(actual$time[1:3], c("0:00", "0:07", "0:41"))
    expect_equal(actual$event[1:3], c("faceoff", "shot", "hit"))
  })
})

with_mock_API({
  test_that("mlb play by play lineup parser works", {
    # mlb data
    resp <- game_pbp("mlb", "20170523-LAA-TB", season = "2017-regular")

    actual <- parse_game_pbp(resp$content, "mlb")

    expect_equal(nrow(actual), 70L)
    expect_equal(ncol(actual), 5L)
    expect_equal(actual$inning[1:3], rep("1", 3L))
    expect_equal(actual$inning_half[1:3], rep("top", 3L))
    expect_equal(actual$batting_team[1:3], rep("124", 3L))
    expect_equal(actual$atbat_id[1:3], 1L:3L)
  })
})
