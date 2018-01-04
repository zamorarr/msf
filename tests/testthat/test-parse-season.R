context("parse-season")

test_that("player injury parser works", {
    # nba data
    resp <- with_mock_test(player_injuries("nba", season = "2017-2018-regular"))
    j <- resp$content

    actual <- parse_player_injuries(j)

    expect_equal(nrow(actual), 61L)
    expect_equal(ncol(actual), 4L)
    expect_equal(actual$id[1:3], c("9299", "9178", "10102"))
    expect_equal(actual$team_id[1:3], c("110", "85", "83"))
    expect_equal(actual$injury[1:3], c("fibula fracture", "shin splints", "orbital fracture"))
    expect_equal(actual$status[1:3], rep("Out", 3))
})

test_that("player cumulative stats parser works", {
    # nba data
    resp <- with_mock_test(cumulative_player_stats("nba", season = "2017-2018-regular"))

    actual <- parse_cumulative_stats(resp$content)

    expect_equal(nrow(actual), 711L)
    expect_equal(ncol(actual), 74L)
    expect_equal(actual$id[1:3], c("10138", "9466", "9390"))
    expect_equal(actual$abbreviation[1:3], c("OKL", "BRO", "OKL"))
    expect_equal(actual$fg2ptatt[1:3], c(40, 39, 287))
    expect_equal(actual$ast[1:3], c(13, 19, 28))
})

test_that("latest updates parser works", {
  # mlb data
  resp <- with_mock_test(latest_updates("mlb", season = "latest"))

  actual <- parse_latest_updates(resp$content)

  expect_equal(nrow(actual), 336L)
  expect_equal(ncol(actual), 4L)
  expect_equal(actual$feed[1:3], rep("daily_game_schedule", 3L))
  expect_equal(actual$feed_type[1:3], rep("date", 3L))
  expect_equal(actual$feed_for[1:3], c("2017-10-05", "2017-10-05", "2017-10-06"))
  expect_equal(as.integer(table(actual$feed_type)), c(183L, 114L, 39L))
})
