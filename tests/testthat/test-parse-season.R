context("parse-season")

with_mock_API({
  test_that("player injury parser works", {
    # nba data
    resp <- player_injuries("nba", season = "2017-2018-regular")
    j <- resp$content

    actual <- parse_player_injuries(j)

    expect_equal(nrow(actual), 61L)
    expect_equal(ncol(actual), 4L)
    expect_equal(actual$id[1:3], c("9299", "9178", "10102"))
    expect_equal(actual$team_id[1:3], c("110", "85", "83"))
    expect_equal(actual$injury[1:3], c("fibula fracture", "shin splints", "orbital fracture"))
    expect_equal(actual$status[1:3], rep("Out", 3))
  })
})

with_mock_API({
  test_that("player cumulative stats parser works", {
    # nba data
    resp <- cumulative_player_stats("nba", season = "2017-2018-regular")

    actual <- parse_cumulative_stats(resp$content)

    expect_equal(nrow(actual), 711L)
    expect_equal(ncol(actual), 74L)
    expect_equal(actual$id[1:3], c("10138", "9466", "9390"))
    expect_equal(actual$abbreviation[1:3], c("OKL", "BRO", "OKL"))
    expect_equal(actual$fg2ptatt[1:3], c(40, 39, 287))
    expect_equal(actual$ast[1:3], c(13, 19, 28))
  })
})
