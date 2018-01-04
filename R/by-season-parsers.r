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
#' @export
#' @examples
#' \dontrun{
#' resp <- mlb_cumulative_player_stats()
#' stats <- parse_cumulative_stats(resp$content)
#' }
parse_cumulative_stats <- function(json) {
  playerstats <- json[["cumulativeplayerstats"]][["playerstatsentry"]]

  # player data
  player_df <- purrr::map(playerstats, "player")
  player_df <- tibble::as_tibble(transpose_and_simplify(player_df))

  # team data
  team_df <- purrr::map(playerstats, "team")
  team_df <- tibble::as_tibble(transpose_and_simplify(team_df))
  team_df$TeamID = team_df$ID
  team_df$ID <- NULL

  # stats
  stats <- purrr::map(playerstats, "stats")
  stats_df <- purrr::map(stats, ~ purrr::map(.x, "#text"))
  stats_df <- tibble::as_tibble(transpose_and_simplify(stats_df))
  stats_df[] <- purrr::map(stats_df, as.double)

  df <- cbind(player_df, team_df, stats_df)
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
  players <- purrr::map(playerinjuries, "player")
  player_df <- tibble::as_tibble(transpose_and_simplify(players))

  # team data
  teams <- purrr::map(playerinjuries, "team")
  team_df <- tibble::as_tibble(transpose_and_simplify(teams))

  # injury
  injuries <- purrr::map_chr(playerinjuries, "injury")
  injury_status <- str_match(injuries, "(.*) \\((.*)\\)")
  injury <- injury_status[[2]]
  status <- injury_status[[3]]

  # return data frame
  tibble::tibble(id = player_df[["ID"]], team_id = team_df[["ID"]],
                 injury = injury, status = status)
}

#' Parse latest updates
#'
#' Parse latest updates into a tidy data frame.
#' This function is pretty slow right now. Needs to be sped up in the future.
#' @param json content from response
#' @export
#' @examples
#' \dontrun{
#' resp <- latest_updates("nba")
#' updates <- parse_latest_updates(resp$content)
#' }
parse_latest_updates <- function(json) {
  feedentries <- json[["latestupdates"]][["feedentry"]]

  # feeds
  feeds <- purrr::map(feedentries, "feed")
  feed_abbr <- purrr::map_chr(feeds, "Abbreviation")

  # separate types (forDate, forGame, None)
  feeds_by_date <- purrr::keep(feedentries, ~ "forDate" %in% names(.x))
  feeds_by_game <- purrr::keep(feedentries, ~ "forGame" %in% names(.x))
  feeds_by_other <- purrr::keep(feedentries, ~ !any(c("forDate", "forGame") %in% names(.x)))

  # last updated
  df_date <- parse_latest_updates_by_date(feeds_by_date)
  df_game <- parse_latest_updates_by_game(feeds_by_game)
  df_other <- parse_latest_updates_by_other(feeds_by_other)

  rbind(df_date, df_game, df_other)
}

#' @keywords internal
parse_latest_updates_by_date <- function(json) {

  # get fields
  feed <- tolower(purrr::map_chr(json, c("feed", "Abbreviation"), .default = NA_character_))
  forDate <- purrr::map(json, "forDate")

  # parse fields
  res <- purrr::map2(feed, forDate, parse_fordate)

  # combine results
  df <- do.call(rbind, res)
  df[!is.na(df$last_updated),]
}

#' @keywords internal
parse_fordate <- function(feed, forDate) {
  nm <- names(forDate)

  # if forDate field is just 2 named elements then its a single entry
  if (!is.null(nm) && identical(sort(nm), c("forDate", "lastUpdatedOn"))) { # not nested
    last_updated <- forDate$lastUpdatedOn
    date <- forDate$forDate
  } else { # nested forDates
    last_updated <- purrr::map_chr(forDate, "lastUpdatedOn", .default = NA_character_)
    date <- purrr::map_chr(forDate, "forDate", .default = NA_character_)
  }

  tibble::tibble(feed = feed, last_updated = last_updated, feed_type = "date", feed_for = date)
}

#' @keywords internal
parse_latest_updates_by_game <- function(json) {
  # get fields
  feed <- tolower(purrr::map_chr(json, c("feed", "Abbreviation"), .default = NA_character_))
  forGame <- purrr::map(json, "forGame")

  # parse fields
  res <- purrr::map2(feed, forGame, parse_forgame)

  # combine results
  do.call(rbind, res)
}

#' @keywords internal
parse_forgame <- function(feed, forGame) {
  nm <- names(forGame)

  # if forGame field is just 2 named elements then its a single entry
  if (!is.null(nm) && identical(sort(nm), c("forGame", "lastUpdatedOn"))) { # not nested
    last_updated <- forGame$lastUpdatedOn
    game <- forGame$game
  } else { # nested forDates
    last_updated <- purrr::map_chr(forGame, "lastUpdatedOn", .default = NA_character_)
    game <- purrr::map_chr(forGame, ~ .x[["game"]][[1]][["ID"]], .default = NA_character_)
  }

  tibble::tibble(feed = feed, last_updated = last_updated, feed_type = "game", feed_for = game)
}

#' @keywords internal
parse_latest_updates_by_other <- function(json) {
  feed <- tolower(purrr::map_chr(json, c("feed", "Abbreviation"), .default = NA_character_))
  last_updated <- purrr::map_chr(json, "lastUpdatedOn", .default = NA_character_)

  tibble::tibble(feed = feed, last_updated = last_updated, feed_type = "other", feed_for = NA)
}
