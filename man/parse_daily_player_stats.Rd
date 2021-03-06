% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by-date-parsers.r
\name{parse_daily_player_stats}
\alias{parse_daily_player_stats}
\title{Parse player stats}
\usage{
parse_daily_player_stats(json)
}
\arguments{
\item{json}{content from response}
}
\description{
Parse player stats
}
\examples{
\dontrun{
resp <- daily_player_stats("nba", as.Date("2017-11-16"), season = "2017-2018-regular")
parse_daily_player_stats(resp$content)
}
}
