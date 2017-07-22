#' MLB Full Game Schedule
#'
#' A list of all games to be played for the entire season.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @export
mlb_full_game_schedule <- function(season = "current") {
  path <- paste0(c("mlb", season, "full_game_schedule.json"), collapse = "/")
  msf_api(path)
}

#' List of active players
#' @param season ex. 2017-regular
#' @export
mlb_active_players <- function(season = "current") {
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
#'
#' @param season ex. 2017-regular
#' @export
mlb_latest_updates <- function(season = "current") {
  path <- paste0(c("mlb", season, "latest_updates.json"), collapse = "/")
  msf_api(path)
}





