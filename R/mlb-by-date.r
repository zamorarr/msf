#' @rdname daily_dfs
#' @export
mlb_daily_dfs <- function(date = Sys.Date(), season = "current") {
  daily_dfs("mlb", date, season)
}

#' @rdname daily_player_stats
#' @export
mlb_daily_player_stats <- function(date = Sys.Date(), season = "current") {
  daily_player_stats("mlb", date, season)
}

#' @rdname daily_game_schedule
#' @export
mlb_daily_game_schedule <- function(date = Sys.Date(), season = "current") {
  daily_game_schedule("mlb", date, season)
}

#' @rdname scoreboard
#' @export
mlb_scoreboard <- function(date = Sys.Date(), season = "current") {
  scoreboard("mlb", date, season)
}
