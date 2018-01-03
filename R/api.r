#' MySportsFeeds API
#'
#' @param path path to pull
#' @param query query parameters
msf_api <- function(path, query = NULL) {
  # url
  baseurl <- "https://api.mysportsfeeds.com"
  path <- paste0("v1.1/pull/", path)
  url <- httr::modify_url(baseurl, path = path, query = query)

  # authentication
  username <- Sys.getenv("MYSPORTSFEEDS_USER")
  password <- Sys.getenv("MYSPORTSFEEDS_PASSWORD")

  if (nchar(username) == 0 | nchar(password) == 0) {
    stop("Please set MYSPORTSFEED_USER and MYSPORTSFEED_PASSWORD envrionment variables", call. = FALSE)
  }

  # get data
  resp <- httr::GET(url, httr::authenticate(username, password))

  # get content
  page <- httr::content(resp, "text", encoding = "utf-8")

  # check errors
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "MySportsFeeds API request failed [%s]\n[%s]",
        httr::status_code(resp), url
      ),
      call. = FALSE
    )
  }

  # check data type
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse content
  json <- jsonlite::fromJSON(page, simplifyVector = FALSE)

  # return S3 object
  structure(
    list(
      content = json,
      path = path,
      response = resp
    ),
    class = "msf_api"
  )
}

#' @export
print.msf_api <- function(x, ...) {
  cat("<MySportsFeeds ", x[["path"]], ">\n", sep = "")
  str(x[["content"]],2)
  invisible(x)
}
