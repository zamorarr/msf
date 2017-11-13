#' NBA Daily Fantasy Stats
#'
#' A list of players, along with their DFS salaries and actual fantasy points.
#' @param season string, ex. "2016-2017-regular" | "2017-playoff"
#' @param date string, ex. "20161104"
#' @export
nba_daily_dfs <- function(season, date) {
  path <- paste0(c("nba", season, "daily_dfs.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' NBA Player Stats
#'
#' A list of player stats totals for those players who particpated in games on a given day.
#' @param season string, ex. "2016-2017-regular" | "2017-playoff"
#' @param date string, ex. "20161104"
#' @export
nba_daily_player_stats <- function(season, date) {
  path <- paste0(c("nba", season, "daily_player_stats.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' NBA Game Boxscore
#'
#' A boxscore of scoring plays and player stats, for a game.
#' @param season string, ex. "2016-2017-regular" | "2017-playoff"
#' @param gameid string, "33941"
#' @export
nba_game_boxscore <- function(season, gameid) {
  path <- paste0(c("nba", season, "game_boxscore.json"), collapse = "/")
  query <- list(gameid = gameid)
  msf_api(path, query)
}


