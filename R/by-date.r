#' MySportsFeeds API calls by date
#'
#' @param req data to request
#' @param sport mlb | nfl | nba | etc..
#' @param season ex. 2017-regular | current
#' @param date date, ex. as.Date("2017-05-08")
msf_by_date <- function(req, sport, date, season) {
  path <- paste0(c(sport, season, req), collapse = "/")
  query <- list(fordate = msf_date(date))
  msf_api(path, query)
}

#' Daily Fantasy Stats
#'
#' A list of players, along with their DFS salaries and actual fantasy points.
#' @inheritParams msf_by_date
daily_dfs <- function(sport, date = Sys.Date(), season = "current") {
  msf_by_date("daily_dfs.json", sport, date, season)
}

#' Player Stats
#'
#' A list of player stats totals for those players who particpated in games on a given day.
#' @inheritParams msf_by_date
daily_player_stats <- function(sport, date = Sys.Date(), season = "current") {
  msf_by_date("daily_player_stats.json", sport, date, season)
}

#' Daily Game Schedule
#'
#' The daily game schedule
#' @inheritParams msf_by_date
daily_game_schedule <- function(sport, date = Sys.Date(), season = "current") {
  msf_by_date("daily_game_schedule.json", sport, date, season)
}

#' Scoreboard
#'
#' Scores and status for all games on a given day.
#' @inheritParams msf_by_date
scoreboard <- function(sport, date = Sys.Date(), season = "current") {
  msf_by_date("scoreboard.json", sport, date, season)
}
