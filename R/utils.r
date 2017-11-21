`%||%` <- function(x, y) {
  if(is.null(x) || length(x) == 0) y else x
}

msf_date <- function(date) {
  stopifnot(lubridate::is.Date(date))
  format(date, "%Y%m%d")
}

delay_by <- function(delay, f) {
  function(...) {
    res <- f(...)
    Sys.sleep(delay)
    res
  }
}

camel_to_underscore <- function(x) {
  gsub(" ", "_", trimws(tolower(gsub("([[:upper:]])", " \\1", x))))
}
