#' MySportsFeeds API calls by date
#'
#' If you provide multiple dates it will execute with a delay between each query.
#' This is to ensure you obey limit rates but you are free to lower the parameter if you don't
#' think your queries will reach the throttle limits.
#'
#' @param feed feed to request
#' @param sport mlb | nfl | nba | etc..
#' @param season ex. 2017-regular | current
#' @param date date, ex. as.Date("2017-05-08")
#' @param delay number of seconds between queries
#' @param ... additional parameters passed to \code{msf_by_date} such as date or season
#'
#' @examples
#' \dontrun{
#' resp <- daily_dfs("nba", c(Sys.Date(), Sys.Date() - 1), delay = 3)
#' resp <- daily_game_schedule("nhl")
#' resp <- daily_player_stats("mlb", as.Date("2017-06-06"), season = "2017-regular")
#' }
msf_by_date <- function(feed, sport, date = Sys.Date(), season = "current", delay = 1) {
  stopifnot(length(feed) == 1L, length(sport) == 1L, length(season) == 1L)
  path <- paste0(c(sport, season, feed), collapse = "/")

  if (length(date) > 1) {
    queries <- purrr::map(date, ~ list(fordate = msf_date(.x)))
    result <- purrr::map(queries, ~ delay_by(delay, msf_api)(path, .x))
    result <- purrr::map2(result, date, function(r, d) {r[["name"]] <- d; r})
    names(result) <- date
  } else {
    query <- list(fordate = msf_date(date))
    result <- msf_api(path, query)
    result$name <- date
  }

  result
}

#' @describeIn msf_by_date The current season for a given date (or today), along with supported team/player stats.
#' @export
current_season <- function(sport, ...) {
  msf_by_date("current_season.json", sport, ...)
}

#' @describeIn msf_by_date A list of players, along with their DFS salaries and actual fantasy points.
#' @export
daily_dfs <- function(sport, ...) {
  msf_by_date("daily_dfs.json", sport, ...)
}

#' @describeIn msf_by_date The daily game schedule
#' @export
daily_game_schedule <- function(sport, ...) {
  msf_by_date("daily_game_schedule.json", sport, ...)
}

#' @describeIn msf_by_date A list of player stats totals for those players who particpated in games on a given day.
#' @export
daily_player_stats <- function(sport, ...) {
  msf_by_date("daily_player_stats.json", sport, ...)
}

#' @describeIn msf_by_date A list of players, along with details, currently assigned to a team's roster.
#' @export
roster_players <- function(sport, ...) {
  msf_by_date("roster_players.json", sport, ...)
}

#' @describeIn msf_by_date Scores and status for all games on a given day.
#' @export
scoreboard <- function(sport, ...) {
  msf_by_date("scoreboard.json", sport, ...)
}
