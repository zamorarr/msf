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


