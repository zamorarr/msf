#' MySportsFeeds API calls by game
#'
#' @param req data to request
#' @param sport mlb | nfl | nba | etc..
#' @param gameid string, "40265"
#' @param season string, ex. "2017-regular" | "2017-playoff"
msf_by_game <- function(req, sport, gameid, season) {
  path <- paste0(c(sport, season, req), collapse = "/")
  query <- list(gameid = gameid)
  msf_api(path, query)
}

#' Game Boxscore
#'
#' A boxscore of scoring plays and player stats, for a game.
#' @inheritParams msf_by_game
game_boxscore <- function(sport, gameid, season = "current") {
  msf_by_game("game_boxscore.json", sport, gameid, season)
}

#' Game Play-by-Play
#'
#' All plays that have occurred for a game.
#' @inheritParams msf_by_game
game_pbp <- function(sport, gameid, season = "current") {
  msf_by_game("game_playbyplay.json", sport, gameid, season)
}

#' Download starting lineups
#'
#' @inheritParams msf_by_game
game_starting_lineup <- function(sport, gameid, season = "current") {
  msf_by_game("game_startinglineup.json", sport, gameid, season)
}
