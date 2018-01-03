#' MySportsFeeds API calls by team
#'
#' If you provide multiple teams it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param team team id or abbreviation
#' @param season ex. 2017-regular | current
#' @param delay number of seconds between queries
#' @param ... additional parameters passed to \code{msf_by_team} such as season
#'
#' @examples
#' \dontrun{
#' resp <- team_gamelogs("nba", c("bos", "atl"))
#' }
msf_by_team <- function(feed, sport, team, season = "current", delay = 1) {
  stopifnot(length(feed) == 1L, length(sport) == 1L, length(season) == 1L)
  path <- paste0(c(sport, season, feed), collapse = "/")

  if (length(team) > 1) {
    queries <- purrr::map(team, ~ list(team = .x))
    result <- purrr::map(queries, ~ delay_by(delay, msf_api)(path, .x))
    result <- purrr::map2(result, team, function(r, t) {r[["name"]] <- t; r})
    names(team) <- team
  } else {
    query <- list(team = team)
    result <- msf_api(path, query)
    result$name <- team
  }

  result
}

#' @describeIn msf_by_team Game logs for one or more teams
#' @export
team_gamelogs <- function(sport, team, ...) {
  msf_by_team("team_gamelogs.json", sport, team, ...)
}

#' @describeIn msf_by_team Game logs for one or more players
#' @export
player_gamelogs <- function(sport, team, ...) {
  msf_by_team("player_gamelogs.json", sport, team, ...)
}

