#' Write MySportsFeeds JSON to disk
#'
#' Files will be named by the name variable in the object. Ex. resp$name
#' @param resp response or list of reponses
#' @param path path to directory
#' @export
write_msf <- function(resp, path) {
  stopifnot(length(path) == 1L)
  is_dir <- file.info(path)$isdir
  stopifnot(is_dir)

  if (class(resp) == "msf_api") {
    filename <- paste0(resp$name, ".json")
    filepath <- file.path(path, filename)
    jsonlite::write_json(resp$content, filepath)
  } else {
    filenames <- purrr::map_chr(resp, "name")
    filenames <- paste0(filenames, ".json")
    filepaths <- file.path(path, filenames)
    purrr::walk2(resp, filepaths, ~ jsonlite::write_json(.x$content, .y))
  }
}
