#' MySportsFeeds API calls by season
#'
#' If you provide multiple seasons it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param season ex. 2017-regular | current
#' @param delay number of seconds between queries
#' @param ... additional parameters passed to \code{msf_by_season} such as season
#'
#' @examples
#' \dontrun{
#' resp <- full_game_schedule("nfl", c("latest", "2016-2017-regular"), delay = 3)
#' }
msf_by_season <- function(feed, sport, season = "current", delay = 5) {
  stopifnot(length(feed) == 1L, length(sport) == 1L)
  path <- paste(sport, season, feed, sep = "/")

  if (length(season) > 1) {
    result <- purrr::map(path, ~ delay_by(delay, msf_api)(.x))
    result <- purrr::map2(result, season, function(r, s) {r[["name"]] <- s; r})
    names(result) <- season
  } else {
    result <- msf_api(path)
    result$name <- season
  }

  result
}

#' @describeIn msf_by_season List of active players
#' @export
active_players <- function(sport, ...) {
  msf_by_season("active_players.json", sport, ...)
}

#' @describeIn msf_by_season A list of player stats totals for all roster players, summarized by their latest team.
#' @export
cumulative_player_stats <- function(sport, ...) {
  msf_by_season("cumulative_player_stats.json", sport, ...)
}

#' @describeIn msf_by_season A list of all games to be played for the entire season.
#' @export
full_game_schedule <- function(sport, ...) {
  msf_by_season("full_game_schedule.json", sport, ...)
}


#' @describeIn msf_by_season This feed lists the last updated date/time for each available feed.
#' @export
latest_updates <- function(sport, ...) {
  msf_by_season("latest_updates.json", sport, ...)
}

#' @describeIn msf_by_season Game logs for one or more players
#' @export
player_gamelogs <- function(sport, ...) {
  # need to also have a query that specifies the players or games to return
  msf_by_season("player_gamelogs.json", sport, ...)
}

#' @describeIn msf_by_season A list of all currently injured players.
#' @export
player_injuries <- function(sport, ...) {
  msf_by_season("player_injuries.json", sport, ...)
}

