#' MySportsFeeds API calls by game
#'
#' If you provide multiple gameids it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param gameid strings, "40265" or c("40265", "11111") or "20171015-SF-WAS"
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param delay number of seconds between queries
#' @param ... additional parameters passed to \code{msf_by_game} such as season or delay
#'
#' @examples
#' \dontrun{
#' resp <- game_starting_lineup("nfl", "20171015-SF-WAS")
#' resp <- game_boxscore("nba", c("20171026-BOS-MIL", "20171109-LAL-WAS"), delay = 3)
#' resp <- game_pbp("nhl", "20161215-FLO-WPJ", season = "2016-2017-regular")
#' }
msf_by_game <- function(feed, sport, gameid, season = "current", delay = 5) {
  stopifnot(length(feed) == 1L, length(sport) == 1L, length(season) == 1L)
  path <- paste0(c(sport, season, feed), collapse = "/")

  if (length(gameid) > 1) {
    queries <- purrr::map(gameid, ~ list(gameid = .x))
    result <- purrr::map(queries, ~ delay_by(delay, msf_api)(path, .x))
    result <- purrr::map2(result, gameid, function(r, g) {r[["name"]] <- g; r})
    names(result) <- gameid
  } else {
    query <- list(gameid = gameid)
    result <- msf_api(path, query)
    result$name <- gameid
  }

  result
}

#' @describeIn msf_by_game A boxscore of scoring plays and player stats, for a game.
#' @export
game_boxscore <- function(sport, gameid, ...) {
  msf_by_game("game_boxscore.json", sport, gameid, ...)
}

#' @describeIn msf_by_game All plays that have occurred for a game.
#' @export
game_pbp <- function(sport, gameid, ...) {
  msf_by_game("game_playbyplay.json", sport, gameid, ...)
}

#' @describeIn msf_by_game Expected and Actual starting lineup for both teams of a game.
#' @export
game_starting_lineup <- function(sport, gameid, ...) {
  msf_by_game("game_startinglineup.json", sport, gameid, ...)
}
