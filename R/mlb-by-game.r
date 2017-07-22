#' MLB Game Boxscore
#'
#' A boxscore of scoring plays and player stats, for a game.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param gameid string, "40265"
#' @export
mlb_game_boxscore <- function(gameid, season = "current") {
  path <- paste0(c("mlb", season, "game_boxscore.json"), collapse = "/")
  query <- list(gameid = gameid)
  msf_api(path, query)
}

#' MLB Game Play-by-Play
#'
#' All plays that have occurred for a game.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param gameid string, "40265"
#' @export
mlb_game_pbp <- function(gameid, season = "current") {
  path <- paste0(c("mlb", season, "game_playbyplay.json"), collapse = "/")
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
