#' Read MySportsFeeds JSON from disk
#'
#' @param path path to json file
#' @export
read_msf <- function(path) {
  #assertive.files::assert_all_are_existing_files(path)
  stopifnot(file.exists(path))
  jsonlite::read_json(path, simplifyVector = TRUE, simplifyMatrix = FALSE,
                      simplifyDataFrame = FALSE)

}
