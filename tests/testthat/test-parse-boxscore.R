context("parse-boxscore")

with_mock_API({
  test_that("nhl boxscore parser works", {
    # nhl data
    resp <- game_boxscore("nhl", "20171004-TOR-WPJ")
    j <- resp$content

    actual <- parse_boxscore(j)
    col_types <- vapply(actual[4:39], class, FUN.VALUE = character(1L))

    expect_equal(nrow(actual), 39L)
    expect_equal(ncol(actual), 39L)
    expect_true(all(col_types == "numeric"))
  })
})


with_mock_API({
  test_that("mlb boxscore parser works", {
    # mlb data
    resp <- game_boxscore("mlb", "20170402-NYY-TB", season = "2017-regular")
    j <- resp$content

    actual <- parse_boxscore(j)
    col_types <- vapply(actual[4:135], class, FUN.VALUE = character(1L))

    expect_equal(nrow(actual), 30L)
    expect_equal(ncol(actual), 135L)
    expect_true(all(col_types == "numeric"))
  })
})

with_mock_API({
  test_that("nfl boxscore parser works", {
    # nfl data
    resp <- game_boxscore("nfl", "20170924-KC-LAC", season = "2017-regular")
    j <- resp$content

    actual <- parse_boxscore(j)
    col_types <- vapply(actual[4:139], class, FUN.VALUE = character(1L))

    expect_equal(nrow(actual), 87L)
    expect_equal(ncol(actual), 139L)
    expect_true(all(col_types == "numeric"))
  })
})

with_mock_API({
  test_that("nfl boxscore parser skips players when they have NULL stats", {
    # nfl data
    #j <- read_msf("nfl-boxscore-with-null-stats.json")
    resp <- game_boxscore("nfl", "20171008-LAC-NYG", season = "2017-regular")
    j <- resp$content

    actual <- parse_boxscore(j)
    col_types <- vapply(actual[4:139], class, FUN.VALUE = character(1L))

    expect_equal(nrow(actual), 88L) # originally had 89
    expect_equal(ncol(actual), 139L)
    expect_true(all(col_types == "numeric"))
  })
})
