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

#' Parse json of game schedule
#'
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- mysportsfeeds::mlb_daily_game_schedule()
#' games <- parse_daily_game_schedule(resp$json)
#' }
parse_game_schedule <- function(json, nm = "dailygameschedule") {
  games <- json[[nm]][["gameentry"]]

  # metadata
  game_id <- purrr::map_chr(games, "id")
  location <- purrr::map_chr(games, "location")

  # datetime
  date <- purrr::map_chr(games, "date")
  time <- purrr::map_chr(games, "time")
  dt <- as.POSIXct(paste(date, time), tz = "America/New_York", format = "%Y-%m-%d %I:%M%p")

  # teams
  away_id <- purrr::map_chr(games, c("awayTeam", "ID"))
  home_id <- purrr::map_chr(games, c("homeTeam", "ID"))

  tibble::tibble(game_id = game_id, away_id, home_id, dt = dt, location = location)
}

#' Parse fantasy salaries for today's games
#'
#' @param json content from response
#' @param site draftkings or fanduel
#' @export
#' @examples
#' \dontrun{
#' resp <- mysportsfeeds::mlb_daily_dfs()
#' salaries <- parse_daily_dfs(resp$json)
#' }
parse_daily_dfs <- function(json, site = c("draftkings", "fanduel")) {
  dfs <- json[["dailydfs"]][["dfsEntries"]]
  if (is.null(dfs)) return(NULL)

  # get site
  site <- match.arg(site)
  sites <- tolower(purrr::map_chr(dfs, "dfsType"))
  site_index <- which(sites == site)
  #stopifnot(length(site_index) == 1L)
  if (length(site_index) != 1L) return(NA)

  # dfs data
  dfs <- dfs[[site_index]][["dfsRows"]]

  # player
  id <- purrr::map_chr(dfs, c("player", "ID"), .null = NA)
  fname <- purrr::map_chr(dfs, c("player", "FirstName"), .null = NA)
  lname <- purrr::map_chr(dfs, c("player", "LastName"), .null = NA)
  position <- purrr::map_chr(dfs, c("player", "Position"), .null = NA)
  #dfs_id <- purrr::map_chr(dfs, c("player", "dfsSourceId"), .null = NA)

  # team
  team_id <- purrr::map_chr(dfs, c("team", "ID"))
  team <- purrr::map_chr(dfs, c("team", "Abbreviation"))

  # game
  game_id <- purrr::map_chr(dfs, c("game", "id"), .null = NA)

  # salary
  salary <- as.integer(purrr::map_chr(dfs, "salary" ))

  # fantasy points
  fpts <- as.double(purrr::map_chr(dfs, "fantasyPoints", .null = NA))

  tibble::tibble(id = id, team_id = team_id, game_id = game_id,
                 fname = fname, lname = lname, team = team, position = position,
                 salary = salary, fpts = fpts)#, dfs_id = dfs_id)
}

