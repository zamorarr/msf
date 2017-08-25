#' @rdname full_game_schedule
#' @export
nfl_full_game_schedule <- function(season = "current") {
  full_game_schedule("nfl", season)
}

#' @rdname active_players
#' @export
nfl_active_players <- function(season = "current") {
  active_players("nfl", season)
}

#' @rdname player_gamelogs
#' @export
nfl_player_gamelogs <- function(season = "current") {
  player_gamelogs("nfl", season)
}

#' @rdname latest_updates
#' @export
nfl_latest_updates <- function(season = "current") {
  latest_updates("nfl", season)
}

#' @rdname cumulative_player_stats
#' @export
nfl_cumulative_player_stats <- function(season = "current") {
  cumulative_player_stats("nfl", season)
}




