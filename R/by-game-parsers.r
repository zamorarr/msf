#' Parse box scores
#'
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- game_boxscore("nfl", "20170917-ARI-IND", season = "2017-2018-regular")
#' resp <- game_boxscore("nhl", "20171114-BUF-PIT", season = "2017-2018-regular")
#' parse_boxscore(resp$content)
#' }
parse_boxscore <- function(json) {
  gameboxscore <- json[["gameboxscore"]]

  # game data
  game <- gameboxscore[["game"]]
  away_id <- game[["awayTeam"]][["ID"]]
  home_id <- game[["homeTeam"]][["ID"]]

  # player data
  away <- gameboxscore[["awayTeam"]][["awayPlayers"]][["playerEntry"]]
  home <- gameboxscore[["homeTeam"]][["homePlayers"]][["playerEntry"]]
  away_players <- purrr::map_chr(away, c("player", "ID"))
  home_players <- purrr::map_chr(home, c("player", "ID"))

  # stats
  away_stats <- purrr::map(away, "stats")
  home_stats <- purrr::map(home, "stats")
  df_away_stats <- parse_stats(away_stats)
  df_home_stats <- parse_stats(home_stats)

  # data frames
  df_home <- tibble(player_id = home_players, team_id = home_id)
  df_home <- dplyr::bind_cols(df_home, df_home_stats)
  df_away <- tibble(player_id = away_players, team_id = away_id)
  df_away <- dplyr::bind_cols(df_away, df_away_stats)

  dplyr::bind_rows(df_away, df_home)
}

#' Parse starting lineup for a game
#'
#' @param json content from response
#' @param type actual or expected lineup
#' @export
#' @examples
#' \dontrun{
#' resp <- game_starting_lineup("nhl", "20171014-BUF-LAK", season = "2017-2018-regular")
#' resp <- game_starting_lineup("mlb", "20170822-COL-KC", season = "2017-regular")
#' parse_starting_lineup(resp$content, "actual")
#'
#' }
parse_starting_lineup <- function(json, type = c("actual", "expected")) {
  startinglineup <- json[["gamestartinglineup"]]

  # game info
  game_id <- startinglineup[["game"]][["id"]]

  # lineups
  type <- match.arg(type)
  lineups <- purrr::map_dfr(startinglineup[["teamLineup"]], parse_single_lineup, type)
  lineups
}

#' @keywords internal
parse_single_lineup <- function(lineup, type) {
  # team info
  team_id <- lineup[["team"]][["ID"]]

  # player info
  players <- lineup[[type]][["starter"]]
  lineup_position <- purrr::map_chr(players, "position", .null = NA)
  player_ids <- purrr::map_chr(players, c("player", "ID"), .null = NA)

  tibble::tibble(player_id = player_ids, team_id = team_id, lineup_position = lineup_position)
}


#' Parse Play by Play Data
#' @param json list of data
#' @param sport sport
#' @export
#' @examples
#' \dontrun{
#' resp <- game_pbp("nhl", "20161215-FLO-WPJ", season = "2016-2017-regular")
#' parse_game_pbp(resp$content, "nhl")
#' }
parse_game_pbp <- function(json, sport = c(NA, "nba", "nhl", "nfl", "mlb")) {
  sport = match.arg(sport)

  # get plays or at-bats
  if (is.na(sport)) {
    stop("Please provide a sport argument.")
  }

  if (sport == "mlb") {
    plays <- json[["gameplaybyplay"]][["atBats"]][["atBat"]]
  } else {
    plays <- json[["gameplaybyplay"]][["plays"]][["play"]]
  }

  # parse events
  if (sport == "nba") {
    quarter <- purrr::map_chr(plays, "quarter")
    time <- purrr::map_chr(plays, "time")
    event <- purrr::map_chr(plays, ~ names(.x)[3])
    event_data <- purrr::map(plays, 3)
    tibble::tibble(quarter = quarter, time = time, event = event, data = event_data)
  } else if (sport == "nhl") {
    period <- purrr::map_chr(plays, "period")
    time <- purrr::map_chr(plays, "time")
    event <- purrr::map_chr(plays, ~ names(.x)[3])
    event_data <- purrr::map(plays, 3)
    tibble::tibble(period = period, time = time, event = event, data = event_data)
  } else if (sport == "nfl") {
    quarter <- purrr::map_chr(plays, "quarter")
    time <- purrr::map_chr(plays, "time")
    event <- purrr::map_chr(plays, ~ tail(names(.x),1))
    event_data <- purrr::map(plays, tail, 1)
    tibble::tibble(quarter = quarter, time = time, event = event, data = event_data)
  } else if (sport == "mlb") {
    inning <- purrr::map_chr(plays, "inning")
    inning_half <- purrr::map_chr(plays, "inningHalf")
    batting_team <- purrr::map_chr(plays, c("battingTeam", "ID"))

    atbat_id <- seq_along(inning)
    event_data <- purrr::map(plays, "atBatPlay")
    event_data <- purrr::map(event_data, parse_mlb_event)

    results <- tibble::tibble(
      inning = inning, inning_half = inning_half, batting_team = batting_team,
      atbat_id = atbat_id, data = event_data)

    tidyr::unnest(results, data)
  }

}

#' Parse mlb events
#' @param event a nested json event
#' @keywords internal
#parse_mlb_event <- function(event) {
#  event_type <- purrr::map_chr(event, ~ names(head(.x)))
#  event_data <- purrr::map(event, 1)
#  play_id <- seq_along(event_type)
#  tibble::tibble(play_id, event = event_type, data = event_data)
#}


mlb_batting_order <- function(id, position, team_id) {
  is_order <- grepl("BO", position) # batting order values start with BO
  col_type <- dplyr::if_else(is_order, "lineup_order", "position")

  df <- tibble::tibble(id = id, position = position, col_type = col_type)
  df <- dplyr::filter(df, !is.na(id))

  # hack to avoid errors when players are listed at multiple lineup spots
  # simply selects the first instance of that player
  df <- dplyr::arrange(df, id, position)
  df <- dplyr::group_by(df, id, col_type)
  df <- dplyr::slice(df, 1)
  df <- dplyr::ungroup(df)

  df <- tidyr::spread(df, col_type, position)


  # add lineup_order column if not found
  if (!("lineup_order" %in% colnames(df))) df[["lineup_order"]] <- NA_character_

  # batting orders are in the form BO1, BO2, BO3, etc..
  # stopifnot(length(df[["lineup_order"]]) == 9)
  df[["lineup_order"]] <- stringr::str_extract(df[["lineup_order"]], "[0-9]")
  df[["lineup_order"]] <- as.integer(df[["lineup_order"]])

  # add team id
  df[["team_id"]] <- team_id

  df[c("id", "lineup_order", "team_id")]
}


