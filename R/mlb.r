#' MLB Daily Fantasy Stats
#'
#' A list of players, along with their DFS salaries and actual fantasy points.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param date string, ex. "20170508"
#' @export
mlb_daily_dfs <- function(season, date) {
  path <- paste0(c("mlb", season, "daily_dfs.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' MLB Player Stats
#'
#' A list of player stats totals for those players who particpated in games on a given day.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param date string, ex. "20170508"
#' @export
mlb_daily_player_stats <- function(season, date) {
  path <- paste0(c("mlb", season, "daily_player_stats.json"), collapse = "/")
  query <- list(fordate = date)
  msf_api(path, query)
}

#' MLB Full Game Schedule
#'
#' A list of all games to be played for the entire season.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @export
mlb_full_game_schedule <- function(season) {
  path <- paste0(c("mlb", season, "full_game_schedule.json"), collapse = "/")
  msf_api(path)
}

#' MLB Game Boxscore
#'
#' A boxscore of scoring plays and player stats, for a game.
#' @param season string, ex. "2017-regular" | "2017-playoff"
#' @param gameid string, "40265"
#' @export
mlb_game_boxscore <- function(season, gameid) {
  path <- paste0(c("mlb", season, "game_boxscore.json"), collapse = "/")
  query <- list(gameid = gameid)
  msf_api(path, query)
}

#' List of active players
#' @param season ex. 2017-regular
#' @export
mlb_active_players <- function(season) {
  path <- paste0(c("mlb", season, "active_players.json"), collapse = "/")
  resp <- msf_api(path)

  data <- resp$content$activeplayers$playerentry
  players <- purrr::map(data, "player")
  teams <- purrr::map(data, "team")

  id <- purrr::map_chr(players, "ID")
  team_id <- purrr::map_chr(teams, ~ if (is.null(.x)) NA else .x[["ID"]])

  lname <- purrr::map_chr(players, "LastName", .null = NA)
  fname <- purrr::map_chr(players, "FirstName", .null = NA)
  jersey <- purrr::map_chr(players, "JerseyNumber", .null = NA)
  position <- purrr::map_chr(players, "Position", .null = NA)
  height <- purrr::map_chr(players, "Height", .null = NA)
  weight <- purrr::map_chr(players, "Weight", .null = NA)
  birthdate <- purrr::map_chr(players, "BirthDate", .null = NA)
  age <- purrr::map_chr(players, "Age", .null = NA)
  birthcity <- purrr::map_chr(players, "BirthCity", .null = NA)
  birthcountry <- purrr::map_chr(players, "BirthCountry", .null = NA)
  is_rookie <- purrr::map_chr(players, "IsRookie", .null = NA)
  mlb_id <- purrr::map_chr(players, c("externalMapping","ID"), .null = NA)

  player_data <- tibble::tibble(id = id, team_id = team_id, lname = lname,
                                fname = fname, jersey = jersey, position = position,
                                height = height, weight = weight, birthdate = birthdate,
                                age = age, birthcity = birthcity,
                                birthcountry = birthcountry, is_rookie = is_rookie,
                                mlb_id = mlb_id)

  resp$parsed <- player_data
  resp
}


