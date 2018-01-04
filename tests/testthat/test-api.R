context("api")

test_that("API calls fail if user/pass environment variables not set", {
  user <- ""
  password <- ""
  expect_error(with_mock_test(
    player_injuries("nba", season = "2017-2018-regular"),
    user = user, password = password))
})
