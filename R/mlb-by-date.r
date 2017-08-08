#' MLB Daily Fantasy Stats
#'
#' A list of players, along with their DFS salaries and actual fantasy points.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param date string, ex. "20170508"
#' @export
mlb_daily_dfs <- function(date, season = "current") {
  path <- paste0(c("mlb", season, "daily_dfs.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' MLB Player Stats
#'
#' A list of player stats totals for those players who particpated in games on a given day.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param date string, ex. "20170508"
#' @export
mlb_daily_player_stats <- function(date, season = "current") {
  path <- paste0(c("mlb", season, "daily_player_stats.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' Daily MLB Game Schedule
#'
#' The daily game schedule
#' @param season ex. 2017-regular | current
#' @param date date format in Ymd such as 20170601
#' @export
mlb_daily_game_schedule <- function(date = format(Sys.Date(), "%Y%m%d"),
                                    season = "current") {
  path <- paste0(c("mlb", season, "daily_game_schedule.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' Scoreboard
#'
#' Scores and status for all games on a given day.
#' @param season ex. 2017-regular | current
#' @param date date format in Ymd such as 20170601
#' @export
mlb_scoreboard <- function(date = format(Sys.Date(), "%Y%m%d"),
                           season = "current") {
  path <- paste0(c("mlb", season, "scoreboard.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}
