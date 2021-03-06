#' Parse player data from json
#'
#' Across most MySportsFeeds data there is usually a common format for player data.
#' This usually includes the player id, first and last names, and position. This function
#' simply pulls out these fields for you so you don't have to copy code.
#' @param json list of json data
#' @keywords internal
parse_players <- function(json) {
  # can't do transpose + simplify because of occasional NULLs. Can't provide a default
  # behaviour like .null = NA in simplify yet.
  df_players <- purrr::transpose(json)
  df_players[] <- purrr::modify_if(df_players, is.list, replace_null_with_na)
  df_players <- tibble::as_tibble(purrr::simplify_all(df_players))
  df_players

  #id <- purrr::map_chr(json, "ID", .null = NA)
  #fname <- purrr::map_chr(json, "FirstName", .null = NA)
  #lname <- purrr::map_chr(json, "LastName", .null = NA)
  #position <- purrr::map_chr(json, "Position", .null = NA)
  #name <- paste(fname, lname, sep = " ")
  #tibble::tibble(id = id, name = name, position = position)
}

#' Parse team data from json
#'
#' Across most MySportsFeeds data there is usually a common format for team data.
#' This usually includes the team id and abbreviation. This function
#' simply pulls out these fields for you so you don't have to copy code.
#' @param json list of json data
#' @keywords internal
parse_teams <- function(json) {
  team_id <- purrr::map_chr(json, "ID", .null = NA)
  team <- purrr::map_chr(json, "Abbreviation", .null = NA)

  tibble::tibble(team_id = team_id, team = team)
}

#' Parse stats data from json
#'
#' Across most MySportsFeeds data there is usually a common format for stats data.
#' This usually includes a number of numeric fields. This function
#' simply pulls out these fields for you so you don't have to copy code.
#' @param json list of json data
#' @keywords internal
parse_stats <- function(json) {
  df_stats <- purrr::modify_if(json, is.null, ~list(list("#text" = NA)))
  df_stats <- purrr::modify_depth(df_stats, 2, "#text")
  df_stats <- transpose_and_simplify(df_stats)
  df_stats <- purrr::map(df_stats, as.double)

  df <- tibble::as_tibble(df_stats)
  colnames(df) <- camel_to_underscore(colnames(df))
  df
}

transpose_and_simplify <- function(x) {
  names_all <- purrr::map(x, names)
  names_unique <- purrr::reduce(names_all, union)

  transposed <- purrr::transpose(x, .names = names_unique)
  transposed[] <- purrr::modify_if(transposed, is.list, replace_null_with_na)
  purrr::simplify_all(transposed)
}

replace_null_with_na <- function(x) {
  stopifnot(is.list(x))
  modes <- unique(vapply(x, typeof, character(1L)))
  if (length(modes) == 1) {
    x
  } else if (length(modes) == 2 & "NULL" %in% modes) {
    type <- modes[modes != "NULL"]
    are_nulls <- vapply(x, is.null, logical(1L))
    x[are_nulls] <- switch(type, character = NA_character_, integer = NA_integer_, double = NA_real_)
    x
  } else {
    stop("Cannot replace nulls in this list", call. = FALSE)
  }
}
