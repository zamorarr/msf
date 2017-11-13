#' MySportsFeeds API calls by season
#'
#' @param req data to request
#' @param sport mlb | nfl | nba | etc..
#' @param season ex. 2017-regular | current
msf_by_season <- function(req, sport, season) {
  path <- paste0(c(sport, season, req), collapse = "/")
  msf_api(path)
}

#' Full Game Schedule
#'
#' A list of all games to be played for the entire season.
#' @inheritParams msf_by_season
full_game_schedule <- function(sport, season = "current") {
  msf_by_season("full_game_schedule.json", sport, season)
}

#' List of active players
#' @inheritParams msf_by_season
active_players <- function(sport, season = "current") {
  msf_by_season("active_players.json", sport, season)
}

#' Player Game Logs
#'
#' Game logs for one or more players
#' @inheritParams msf_by_season
player_gamelogs <- function(sport, season = "current") {
  # need to also have a query that specifies the players or games to return
  msf_by_season("player_gamelogs.json", sport, season)
}

#' Latest Updates
#'
#' This feed lists the last updated date/time for each available feed.
#' @inheritParams msf_by_season
latest_updates <- function(sport, season = "current") {
  msf_by_season("latest_updates.json", sport, season)
}

#' Cumulative Player Stats
#'
#' A list of player stats totals for all roster players, summarized by their latest team.
#' @inheritParams msf_by_season
cumulative_player_stats <- function(sport, season = "current") {
  msf_by_season("cumulative_player_stats.json", sport, season)
}


#' Player Injuries
#'
#' A list of all currently injured players.
#' @inheritParams msf_by_season
#' @export
player_injuries <- function(sport, season = "current") {
  msf_by_season("player_injuries.json", sport, season)
}
