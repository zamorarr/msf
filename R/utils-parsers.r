parse_players <- function(json) {
  # can't do transpose + simplify because of occasional NULLs. Can't provide a default
  # behaviour like .null = NA in simplify yet.
  id <- purrr::map_chr(json, "ID", .null = NA)
  fname <- purrr::map_chr(json, "FirstName", .null = NA)
  lname <- purrr::map_chr(json, "LastName", .null = NA)
  position <- purrr::map_chr(json, "Position", .null = NA)
  name <- paste(fname, lname, sep = " ")

  tibble::tibble(id = id, name = name, position = position)
}

parse_teams <- function(json) {
  team_id <- purrr::map_chr(json, "ID", .null = NA)
  team <- purrr::map_chr(json, "Abbreviation", .null = NA)

  tibble::tibble(team_id = team_id, team = team)
}

parse_stats <- function(json) {
  stat_values <- purrr::modify_depth(stats, 2, "#text")
  df_stats <- purrr::simplify_all(purrr::transpose(stat_values))
  df_stats[] <- purrr::map(df_stats, as.double)
  tibble::as_tibble(df_stats)
}

