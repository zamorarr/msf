`%||%` <- function(x, y) {
  if(is.null(x) || length(x) == 0) y else x
}

msf_date <- function(date) {
  stopifnot(lubridate::is.Date(date))
  format(date, "%Y%m%d")
}
