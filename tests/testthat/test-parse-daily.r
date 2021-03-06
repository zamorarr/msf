context("parse-daily")

test_that("current season parser works", {
    # nba data
    resp <- with_mock_test(current_season("nba", date = as.Date("2018-01-03")))
    j <- resp$content

    actual <- parse_current_season(j)

    expect_equal(nrow(actual), 1L)
    expect_equal(ncol(actual), 7L)
    expect_equal(nrow(actual$supportedPlayerStats[[1]]), 64)
    expect_equal(ncol(actual$supportedPlayerStats[[1]]), 3)
    expect_equal(nrow(actual$supportedTeamStats[[1]]), 67)
    expect_equal(ncol(actual$supportedTeamStats[[1]]), 3)
})

test_that("nhl daily dfs parser works", {
    # nhl data
    resp <- with_mock_test(daily_dfs("nhl", date = as.Date("2017-11-20"), season = "2017-2018-regular"))
    j <- resp$content

    actual <- parse_daily_dfs(j, "draftkings")
    expect_equal(nrow(actual), 413L)
    expect_equal(ncol(actual), 8L)
})

test_that("nfl daily dfs parser works", {
    # nfl data
    resp <- with_mock_test(daily_dfs("nfl", date = as.Date("2017-11-20"), season = "2017-regular"))
    j <- resp$content

    actual <- parse_daily_dfs(j, "draftkings")
    expect_equal(nrow(actual), 501L)
    expect_equal(ncol(actual), 8L)
})

test_that("daily game schedule parser works", {
    # nfl data
    resp <- with_mock_test(daily_game_schedule("nfl", date = as.Date("2017-11-20"), season = "2017-regular"))
    j <- resp$content

    actual <- parse_daily_game_schedule(j)

    expect_equal(nrow(actual), 1L)
    expect_equal(ncol(actual), 11L)
})

test_that("daily player stats parser works", {
    # nba data
    resp <- with_mock_test(daily_player_stats("nba", date = as.Date("2017-11-16"), season = "2017-2018-regular"))
    j <- resp$content

    actual <- parse_daily_player_stats(j)

    expect_equal(nrow(actual), 43L)
    expect_equal(ncol(actual), 72L)
})

test_that("daily roster players parser works", {
  # nba data
  #j <- read_msf("mlb-roster-players.json")
  resp <- with_mock_test(roster_players("mlb", date = as.Date("2017-11-20"), season = "2017-regular"))
  j <- resp$content

  actual <- parse_roster_players(j)

  expect_equal(nrow(actual), 1896L)
  expect_equal(ncol(actual), 16L)
})
