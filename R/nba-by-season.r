#' @rdname full_game_schedule
#' @export
nba_full_game_schedule <- function(season = "current") {
  full_game_schedule("nba", season)
}

#' @rdname active_players
#' @export
nba_active_players <- function(season = "current") {
  active_players("nba", season)
}
