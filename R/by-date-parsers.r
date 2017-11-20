#' Parse Current Season
#'
#' @param json list of json content
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
  player_df <- tibble::as_tibble(purrr::simplify_all(purrr::transpose(player_stats)))

  # team stats
  team_stats <- season[["supportedTeamStats"]][[1]]
  team_df <- tibble::as_tibble(purrr::simplify_all(purrr::transpose(team_stats)))

  dplyr::mutate(details,
                supportedPlayerStats = list(player_df),
                supportedTeamStats = list(team_df))
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
  df_players <- parse_players(players)
  df_players$dfs_id <- purrr::map_chr(players, "dfsSourceId", .null = NA)

  # team
  teams <- purrr::map(dfs, "team")
  df_teams <- parse_teams(teams)

  # game
  game_id <- purrr::map_chr(dfs, c("game", "id"), .null = NA)

  # salary
  salary <- as.integer(purrr::map_chr(dfs, "salary" ))

  # fantasy points
  fpts <- as.double(purrr::map_chr(dfs, "fantasyPoints", .null = NA))

  result <- dplyr::bind_cols(df_players, df_teams)
  result[c("game_id", "salary", "fpts")] <- list(game_id, salary, fpts)

  result
}

#' Parse game schedules
#'
#' @param json content from response
#' @param nm "dailygameschedule" or "fullgameschedule"
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
  df_away <- parse_teams(away)
  colnames(df_away) <- c("away_id", "away_team")

  home <- purrr::map(games, "homeTeam")
  df_home <- parse_teams(home)
  colnames(df_home) <- c("home_id", "home_team")

  result <- tibble::tibble(game_id = game_id, dt = dt, location = location)
  dplyr::bind_cols(result, df_away, df_home)
}

#' @describeIn parse_game_schedule Parse daily game schedule
#' @export
parse_daily_game_schedule <- function(json) {
  parse_game_schedule(json, "dailygameschedule")
}

#' Parse player stats
#'
#' @param json content from response
#' @examples
#' \dontrun{
#' resp <- daily_player_stats("nba", as.Date("2017-11-16"), season = "2017-2018-regular")
#' parse_daily_player_stats(resp$content)
#' }
parse_daily_player_stats <- function(json) {
  playerstats <- json[["dailyplayerstats"]][["playerstatsentry"]]

  # player data
  players <- purrr::map(playerstats, "player")
  df_players <- parse_players(players)

  # team data
  teams <- purrr::map(playerstats, "team")
  df_teams <- parse_teams(teams)

  # stats
  stats <- purrr::map(playerstats, "stats")
  df_stats <- parse_stats(stats)

  dplyr::bind_cols(df_players, df_teams, df_stats)
}

