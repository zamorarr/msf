#' @rdname game_boxscore
#' @export
mlb_game_boxscore <- function(gameid, season = "current") {
  game_boxscore("mlb", gameid, season)
}

#' @rdname game_pbp
#' @export
mlb_game_pbp <- function(gameid, season = "current") {
  game_pbp("mlb", gameid, season)
}

#' @rdname game_starting_lineup
#' @export
mlb_game_starting_lineup <- function(gameid, season = "current") {
  game_starting_lineup("mlb", gameid, season)
}
