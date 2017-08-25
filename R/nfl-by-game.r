#' @rdname game_boxscore
#' @export
nfl_game_boxscore <- function(gameid, season = "current") {
  game_boxscore("nfl", gameid, season)
}

#' @rdname game_pbp
#' @export
nfl_game_pbp <- function(gameid, season = "current") {
  game_pbp("nfl", gameid, season)
}

#' @rdname game_starting_lineup
#' @export
nfl_game_starting_lineup <- function(gameid, season = "current") {
  game_starting_lineup("nfl", gameid, season)
}
