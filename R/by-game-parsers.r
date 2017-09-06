#' Parse starting lineup for a game
#'
#' @param json content from response
#' @param type actual or expected lineup
#' @param is_mlb whether to parse batting order or not
#' @export
#' @examples
#' \dontrun{
#' resp <- mysportsfeeds::mlb_game_starting_lineup("38494")
#' starters <- mlb_starters(resp$json)
#' }
parse_starting_lineup <- function(json, type = c("expected", "actual"),
                                  is_mlb = FALSE) {
  game <- json[["gamestartinglineup"]][["game"]]
  lineups <- json[["gamestartinglineup"]][["teamLineup"]]

  # expected lineups
  type <- match.arg(type)
  starters <- purrr::map(lineups, c(type, "starter"))
  team_ids <- purrr::map_chr(lineups, c("team", "ID"))

  if(is.null(starters[[1]])) return(NULL) # add better catch for this
  starters_df <- purrr::map2_df(starters, team_ids, extract_lineup, is_mlb)
  starters_df[["game_id"]] <- game[["id"]]

  starters_df
}

#' Extract expected lineup
#'
#' @param x list of expected players in lineup
#' @param team_id id of team
extract_lineup <- function(x, team_id, is_mlb) {
  # player and position
  id <- purrr::map_chr(x, c("player", "ID"), .null = NA)
  pos <- purrr::map_chr(x, c("player", "Position"), .null = NA)

  # more detailed position
  position <- purrr::map_chr(x, "position")

  # lineup order
  if (is_mlb) {
    mlb_batting_order(id, position, team_id)
  } else {
    tibble::tibble(id = id, team_id = team_id, position = position, pos = pos)
  }
}

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

  # batting orders are in the form BO1, BO2, BO3, etc..
  df[["lineup_order"]] <- stringr::str_extract(df[["lineup_order"]], "[0-9]")
  df[["lineup_order"]] <- as.integer(df[["lineup_order"]])
  #stopifnot(length(df[["lineup_order"]]) == 9)

  # add team id
  df[["team_id"]] <- team_id

  df[c("id", "lineup_order", "team_id")]
}




