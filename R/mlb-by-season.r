#' @rdname full_game_schedule
#' @export
mlb_full_game_schedule <- function(season = "current") {
  full_game_schedule("mlb", season)
}

#' @rdname active_players
#' @export
mlb_active_players <- function(season = "current") {
  active_players("mlb", season)
}

#' @rdname player_gamelogs
#' @export
mlb_player_gamelogs <- function(season = "current") {
  player_gamelogs("mlb", season)
}

#' @rdname latest_updates
#' @export
mlb_latest_updates <- function(season = "current") {
  latest_updates("mlb", season)
}

#' @rdname cumulative_player_stats
#' @export
mlb_cumulative_player_stats <- function(season = "current") {
  cumulative_player_stats("mlb", season)
}




