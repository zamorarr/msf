#' Parse Current Season
#'
#' @param json list of json content
#' @export
#' @examples
#' \dontrun{
#' resp <- current_season("nba")
#' parse_current_season(resp$content)
#' }
parse_current_season <- function(json) {
  season <- json[["currentseason"]][["season"]][[1]]

  # details
  details <- tibble::as_tibble(season[["details"]])

  # player stats
  player_stats <- season[["supportedPlayerStats"]][[1]]
  player_df <- tibble::as_tibble(transpose_and_simplify(player_stats))

  # team stats
  team_stats <- season[["supportedTeamStats"]][[1]]
  team_df <- tibble::as_tibble(transpose_and_simplify(team_stats))

  details$supportedPlayerStats <- list(player_df)
  details$supportedTeamStats <- list(team_df)
  details
}

#' Parse fantasy salaries for today's games
#'
#' @param json content from response
#' @param site draftkings or fanduel
#' @export
#' @examples
#' \dontrun{
#' resp <- daily_dfs("nba")
#' parse_daily_dfs(resp$content)
#' }
parse_daily_dfs <- function(json, site = c("draftkings", "fanduel")) {
  dfs <- json[["dailydfs"]][["dfsEntries"]]
  if (is.null(dfs)) return(NULL)

  # get site
  site <- match.arg(site)
  sites <- tolower(purrr::map_chr(dfs, "dfsType"))
  site_index <- which(sites == site)
  if (length(site_index) != 1L) return(NA)

  # dfs data
  dfs <- dfs[[site_index]][["dfsRows"]]

  # player
  players <- purrr::map(dfs, "player")
  players <- purrr::modify_if(players, is.null, ~list(ID = NA_character_))
  df_players <- tibble::as_tibble(transpose_and_simplify(players))
  colnames(df_players) <- paste("player", tolower(colnames(df_players)), sep = "_")

  # team
  teams <- purrr::map(dfs, "team")
  df_teams <- tibble::as_tibble(transpose_and_simplify(teams))
  colnames(df_teams) <- paste("team", tolower(colnames(df_teams)), sep = "_")

  # game
  game_id <- purrr::map_chr(dfs, c("game", "id"), .null = NA)

  # salary
  salary <- as.integer(purrr::map_chr(dfs, "salary"), .null = NA)

  # fantasy points
  fpts <- as.double(purrr::map_chr(dfs, "fantasyPoints", .null = NA))

  result <- cbind(df_players, df_teams)
  result[c("game_id", "salary", "fpts")] <- list(game_id, salary, fpts)

  # tidy data frame
  result$name <- paste(result$player_firstname, result$player_lastname, sep = " ")
  result[c("team", "position")] <- result[c("team_abbreviation", "player_position")]
  result <- result[c("player_id", "team_id", "game_id", "name", "team", "position", "salary", "fpts")]
  result
}

#' Parse game schedules
#'
#' @param json content from response
#' @param nm "dailygameschedule" or "fullgameschedule"
#' @keywords internal
#' @examples
#' \dontrun{
#' resp <- daily_game_schedule("nfl")
#' parse_daily_game_schedule(resp$content)
#' }
parse_game_schedule <- function(json, nm) {
  games <- json[[nm]][["gameentry"]]

  # metadata
  game_id <- purrr::map_chr(games, "id")
  location <- purrr::map_chr(games, "location")

  # datetime
  date <- purrr::map_chr(games, "date")
  time <- purrr::map_chr(games, "time")
  dt <- as.POSIXct(paste(date, time), tz = "America/New_York", format = "%Y-%m-%d %I:%M%p")

  # teams
  away <- purrr::map(games, "awayTeam")
  df_away <- tibble::as_tibble(transpose_and_simplify(away))
  colnames(df_away) <- paste("away", tolower(colnames(df_away)), sep = "_")

  home <- purrr::map(games, "homeTeam")
  df_home <- tibble::as_tibble(transpose_and_simplify(home))
  colnames(df_home) <- paste("home", tolower(colnames(df_home)), sep = "_")

  result <- tibble::tibble(game_id = game_id, dt = dt, location = location)
  cbind(result, df_away, df_home)
}

#' @describeIn parse_game_schedule Parse daily game schedule
#' @export
parse_daily_game_schedule <- function(json) {
  parse_game_schedule(json, "dailygameschedule")
}

#' Parse player stats
#'
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- daily_player_stats("nba", as.Date("2017-11-16"), season = "2017-2018-regular")
#' parse_daily_player_stats(resp$content)
#' }
parse_daily_player_stats <- function(json) {
  playerstats <- json[["dailyplayerstats"]][["playerstatsentry"]]

  # player data
  players <- purrr::map(playerstats, "player")
  df_players <- tibble::as_tibble(transpose_and_simplify(players))
  colnames(df_players) <- paste("player", tolower(colnames(df_players)), sep = "_")

  # team data
  teams <- purrr::map(playerstats, "team")
  df_teams <- tibble::as_tibble(transpose_and_simplify(teams))
  colnames(df_teams) <- paste("team", tolower(colnames(df_teams)), sep = "_")

  # stats
  stats <- purrr::map(playerstats, "stats")
  df_stats <- parse_stats(stats)

  cbind(df_players, df_teams, df_stats)
}


#' Parse roster players
#'
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- roster_players("mlb", season = "2017-regular")
#' parse_roster_players(resp$content)
#' }
parse_roster_players <- function(json) {
  playerentries <- json[["rosterplayers"]][["playerentry"]]

  # player data
  players <- purrr::map(playerentries, "player")
  df_players <- transpose_and_simplify(players)

  # fix some column types
  df_players <- purrr::modify_at(df_players, c("Weight", "Age"), as.integer)
  df_players <- purrr::modify_at(df_players, "IsRookie", as.logical)
  df_players <- purrr::modify_at(df_players, "BirthDate", as.Date, format = "%Y-%m-%d")

  # convert to dataframe
  df_players <- tibble::as_tibble(df_players)
  colnames(df_players) <- paste("player", tolower(colnames(df_players)), sep = "_")

  # team data
  teams <- purrr::map(playerentries, "team")
  teams <- purrr::modify_if(teams, is.null, ~list(ID = NA_character_))
  df_teams <- tibble::as_tibble(transpose_and_simplify(teams))
  colnames(df_teams) <- paste("team", tolower(colnames(df_teams)), sep = "_")

  cbind(df_players, df_teams)
}

#' Parse scoreboard
#' @param json content from response
#' @examples
#' \dontrun{
#' resp <- scoreboard("mlb", date = as.Date("2017-07-24"), season = "2017-regular")
#' parse_scoreboard(resp$content)
#' }
parse_scoreboard <- function(json) {
  stop("Not yet implemented", call. = FALSE)
  gamescores <- json[["scoreboard"]][["gameScore"]]

  # game data
  game_id <- purrr::map_chr(gamescores, c("game", "ID"))

  # unnested data
  df_misc <- purrr::modify(gamescores, ~ .x[-c(1,8)])
  df_misc <- transpose_and_simplify(df_misc)
  df_misc <- tibble::as_tibble(df_misc[-4])
  df_misc <- purrr::modify_at(df_misc, c("isUnplayed", "isInProgress", "isCompleted"), as.logical)
  df_misc <- purrr::modify_at(df_misc, c("awayScore", "homeScore"), as.integer)

  # inning summary
  # what if it's not baseball?
  #inning_summary <- purrr::map(gamescores, c("inningSummary", "inning"))
  inning_summary <- purrr::map(gamescores, ~ tail(.x, 1)[[1]])
  inning_summary <- purrr::map(inning_summary, transpose_and_simplify)
  df_inning <- purrr::map(inning_summary, tibble::as_tibble)

  result <- tibble::tibble(game_id = game_id, inning_summary = df_inning)
  cbind(result, df_misc)
}
