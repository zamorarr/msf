#' Parse active players
#'
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- mysportsfeeds::mlb_active_players()
#' parse_active_players(resp$content)
#' }
parse_active_players <- function(json) {
  players <- json[["activeplayers"]][["playerentry"]]

  # players
  id <- purrr::map_chr(players, c("player", "ID"))
  position <- purrr::map_chr(players, c("player", "Position"))
  fname <- purrr::map_chr(players, c("player", "FirstName"))
  lname <- purrr::map_chr(players, c("player", "LastName"))
  is_rookie <- purrr::map_chr(players, c("player", "IsRookie"))
  is_rookie <- as.logical(is_rookie)

  # team
  team_id <- purrr::map_chr(players, c("team", "ID"), .null = NA)
  team <- purrr::map_chr(players, c("team", "Abbreviation"), .null = NA)

  tibble::tibble(id = id, team_id = team_id, fname = fname, lname = lname,
                 team = team, position = position, is_rookie = is_rookie)
}
