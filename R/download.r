#' Download old data easily
#'
#' Download historical data and write out to file.
#'
#' @param path to save files to
#' @param f function to iterate over, ex. nba_daily_player_stats
#' @param season season to get, ex. "2016-2017-regular"
#' @param start_date,end_date date strings, ex "20161025", "20161031"
#' @keywords internal
#' @examples \dontrun{
#' download_dates("data-raw/daily-dfs/", nba_daily_dfs, "2016-2017-regular", "20161025", "20170412")
#  download_dates("data-raw/daily-player-stats/", nba_daily_player_stats, "2016-2017-regular", "20161025", "20170412")
#' }
download_dates <- function(path, f, season, start_date, end_date, fenv = parent.frame()) {

  start_date <- as.Date(start_date, "%Y%m%d")
  end_date <- as.Date(end_date, "%Y%m%d")
  dates <- format(seq(start_date, end_date, by = 1), "%Y%m%d")

  for(date in dates) {
    download_date(path, f, season, date, fenv)
    # wait a bit - rate throttling.
    # The official rate limit is 100 reqs/5 mins. We'll go even slower
    Sys.sleep(5)
  }

  path
}

#' @keywords internal
download_date <- function(path, f, season, date, fenv = parent.frame()) {
  # create file path
  filename <- paste0(date, "-", deparse(substitute(f, fenv)), ".json")
  filepath <- file.path(path, filename)

  # get data
  resp <- f(season, date)

  # write data to filename
  jsonlite::write_json(resp$content, filepath)
}

#' @export
download_yesterday <- function(path, f, season) {
  yesterday <- format(Sys.Date() - 1, "%Y%m%d")
  download_date(path, f, season, yesterday)
}

#' @export
download_till_yesterday <- function(path, f, season, start_date) {
  yesterday <- format(Sys.Date() - 1, "%Y%m%d")
  download_dates(path, f, season, start_date, yesterday)
}

#' @export
download_games <- function(path, f, season, games, fenv = parent.frame()) {

  for(game in games) {
    download_game(path, f, season, game)
    # wait a bit - rate throttling.
    # The official rate limit is 100 reqs/5 mins. We'll go even slower
    Sys.sleep(5)
  }

  path
}

#' @keywords internal
download_game <- function(path, f, season, gameid, fenv = parent.frame()) {
  # create file path
  filename <- paste0(gameid, "-", deparse(substitute(f, fenv)), ".json")
  filepath <- file.path(path, filename)

  # get data
  resp <- f(season, gameid)

  # write data to filename
  jsonlite::write_json(resp$content, filepath)
}
