#' Download old data easily
#'
#' Download historical data and write out to file.
#'
#' @param path to save files to
#' @param f function to iterate over, ex. nba_daily_player_stats
#' @param season season to get
#' @param start_date,end_date date strings
#' @examples \dontrun{
#' download_dates("data-raw/daily-dfs/", nba_daily_dfs, "2016-2017-regular", "20161025", "20170412")
#  download_dates("data-raw/daily-player-stats/", nba_daily_player_stats, "2016-2017-regular", "20161025", "20170412")
#' }
download_dates <- function(path, f, season = "2016-2017-regular",
                           start_date = "20161025", end_date = "20161031") {

  start_date <- as.Date(start_date, "%Y%m%d")
  end_date <- as.Date(end_date, "%Y%m%d")
  dates <- format(seq(start_date, end_date, by = 1), "%Y%m%d")
  for(date in dates) {
    # create file path
    filename <- paste0(date, "-", deparse(substitute(f)), ".json")
    filepath <- file.path(path, filename)

    # get data
    resp <- f(season, date)

    # write data to filename
    jsonlite::write_json(resp$content, filepath)

    # wait a bit - rate throttling.
    # The official rate limit is 100 reqs/5 mins. We'll go even slower
    Sys.sleep(5)
  }

  path
}

