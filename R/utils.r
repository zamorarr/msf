`%||%` <- function(x, y) {
  if(is.null(x) || length(x) == 0) y else x
}

msf_date <- function(date) {
  stopifnot(methods::is(date, "Date"))
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

str_match <- function(x, pattern) {
  locations <- regexec(pattern, x)
  matches <- regmatches(x, locations)
  transpose_and_simplify(matches)
}
