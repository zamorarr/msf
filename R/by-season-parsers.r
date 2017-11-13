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

#' Parse cumulative stats
#'
#' @param json content from response
#' @param site draftkings or fanduel
#' @export
#' @examples
#' \dontrun{
#' resp <- mlb_cumulative_player_stats()
#' stats <- parse_cumulative_stats(resp$content)
#' }
parse_cumulative_stats <- function(json) {
  playerstats <- json[["cumulativeplayerstats"]][["playerstatsentry"]]

  # player data
  player_df <- purrr::map_df(playerstats, "player")

  # team data
  team_df <- purrr::map_df(playerstats, "team")
  team_df <- dplyr::rename(team_df, "TeamID" = "ID")

  # stats
  stats <- purrr::map(playerstats, "stats")
  stats_df <- purrr::map_df(stats, ~ purrr::map(.x, "#text"))
  stats_df[] <- purrr::map(stats_df, as.double)

  df <- dplyr::bind_cols(player_df, team_df, stats_df)
  colnames(df) <- tolower(colnames(df))

  df
}

#' Parse player injuries
#'
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- player_injuries("nba")
#' injuries <- parse_player_injuries(resp$content)
#' }
parse_player_injuries <- function(json) {
  playerinjuries <- json[["playerinjuries"]][["playerentry"]]

  # player data
  player_df <- purrr::map_df(playerinjuries, "player")
  #print(player_df)

  # team data
  team_df <- purrr::map_df(playerinjuries, "team")

  # injury
  injury <- purrr::map_chr(playerinjuries, "injury")

  #df <- dplyr::bind_cols(player_df, team_df, stats_df)
  tibble::tibble(id = player_df[["ID"]], team_id = team_df[["ID"]], injury = injury)
}
