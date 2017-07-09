#' MLB Daily Fantasy Stats
#'
#' A list of players, along with their DFS salaries and actual fantasy points.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param date string, ex. "20170508"
#' @export
mlb_daily_dfs <- function(season, date) {
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
mlb_daily_player_stats <- function(season, date) {
  path <- paste0(c("mlb", season, "daily_player_stats.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' MLB Full Game Schedule
#'
#' A list of all games to be played for the entire season.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @export
mlb_full_game_schedule <- function(season) {
  path <- paste0(c("mlb", season, "full_game_schedule.json"), collapse = "/")
  msf_api(path)
}

#' MLB Game Boxscore
#'
#' A boxscore of scoring plays and player stats, for a game.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param gameid string, "40265"
#' @export
mlb_game_boxscore <- function(season, gameid) {
  path <- paste0(c("mlb", season, "game_boxscore.json"), collapse = "/")
  query <- list(gameid = gameid)
  msf_api(path, query)
}

#' Download starting mlb starting lineups
#'
#' @param gameid the game id
#' @param season season such as 2017-regular | 2016-playoffs | current
#' @export
mlb_game_starting_lineup <- function(gameid, season = "current") {
  path <- paste0(c("mlb", season, "game_startinglineup.json"), collapse = "/")
  query <- list(gameid = gameid)
  msf_api(path, query)
}

#' List of active players
#' @param season ex. 2017-regular
#' @export
mlb_active_players <- function(season) {
  path <- paste0(c("mlb", season, "active_players.json"), collapse = "/")
  msf_api(path)
}

#' Player Game Logs
#'
#' Game logs for one or more players
#' @param season ex. 2017-regular
#' @export
mlb_player_gamelogs <- function(season = "current") {
  # need to also have a query that specifies the players or games to return
  path <- paste0(c("mlb", season, "player_gamelogs.json"), collapse = "/")
  msf_api(path)
}

#' Latest Updates
#'
#' This feed lists the last updated date/time for each available feed.
mlb_latest_updates <- function(season = "current") {
  path <- paste0(c("mlb", season, "latest_updates.json"), collapse = "/")
  msf_api(path)
}

#' Daily MLB Game Schedule
#'
#' The daily game schedule
#' @param season ex. 2017-regular | current
#' @param date date format in Ymd such as 20170601
#' @export
mlb_daily_game_schedule <- function(season = "current",
                                    date = format(Sys.Date(), "%Y%m%d")) {
  path <- paste0(c("mlb", season, "daily_game_schedule.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}


