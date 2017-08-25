#' @rdname daily_dfs
#' @export
nfl_daily_dfs <- function(date = Sys.Date(), season = "current") {
  daily_dfs("nfl", date, season)
}

#' @rdname daily_player_stats
#' @export
nfl_daily_player_stats <- function(date = Sys.Date(), season = "current") {
  daily_player_stats("nfl", date, season)
}

#' @rdname daily_game_schedule
#' @export
nfl_daily_game_schedule <- function(date = Sys.Date(), season = "current") {
  daily_game_schedule("nfl", date, season)
}

#' @rdname scoreboard
#' @export
nfl_scoreboard <- function(date = Sys.Date(), season = "current") {
  scoreboard("nfl", date, season)
}
