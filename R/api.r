#' MySportsFeeds API
#'
#' @param path path to pull
#' @param query query parameters
#' @importFrom httr modify_url GET authenticate http_type user_agent content http_error
msf_api <- function(path, query = NULL) {
  # url
  baseurl <- "https://www.mysportsfeeds.com"
  path <- paste0("api/feed/pull/", path)
  url <- modify_url(baseurl, path = path, query = query)

  # authentication
  username <- Sys.getenv("MYSPORTSFEEDS_USER")
  password <- Sys.getenv("MYSPORTSFEEDS_PASSWORD")

  # get data
  resp <- GET(url, authenticate(username, password))

  # get content
  page <- content(resp, "text", encoding = "utf-8")

  # check errors
  if (http_error(resp)) {
    html <- xml2::read_html(page)
    error_msg <- xml2::xml_text(xml2::xml_find_all(html, "//body/h1"))
    stop(
      sprintf(
        "MySportsFeeds API request failed [%s]\n%s\n",
        status_code(resp),
        error_msg
      ),
      call. = FALSE
    )
  }

  # check data type
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # parse content
  parsed <- jsonlite::fromJSON(page, simplifyVector = FALSE)

  # return S3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "msf_api"
  )
}

#' @export
print.msf_api <- function(x, ...) {
  cat("<MySportsFeeds ", x$path, ">\n", sep = "")
  str(x$content,2)
  invisible(x)
}
