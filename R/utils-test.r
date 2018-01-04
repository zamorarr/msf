#' Capture requests to tests directory
#'
#' Helper to build up mock fixtures for testing
#' @param expr R expresssion to execute
#' @keywords internal
#' @examples
#' \dontrun{
#' capture_requests2(game_boxscore("nba", "20171027-BRO-NYK"))
#' }
capture_requests2 <- function(expr) {
  httptest::capture_requests(expr, path = "tests/testthat")
}

#' Write json pretty
#'
#' Re-write existing json file with pretty formatting
#'
#' @param path path to json file on disk
#' @keywords internal
#' @examples
#' \dontrun{
#' pretty_json("tests/testthat/api.coinbase.com/v2/accounts.json")
#' }
pretty_json <- function(path) {
  json <- jsonlite::read_json(path)
  jsonlite::write_json(json, path, pretty = TRUE, auto_unbox = TRUE)
}

#' Mock API calls with fake credentials
#'
#' @param expr R expression
#' @param user,password strings
#' @keywords internal
with_mock_test <- function(expr, user = "fakeuser", password = "fakepass") {
    envvars <- c("MYSPORTSFEEDS_USER" = user, "MYSPORTSFEEDS_PASSWORD" = password)
    withr::with_envvar(envvars, httptest::with_mock_API(expr))
}
