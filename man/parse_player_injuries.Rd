% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by-season-parsers.r
\name{parse_player_injuries}
\alias{parse_player_injuries}
\title{Parse player injuries}
\usage{
parse_player_injuries(json)
}
\arguments{
\item{json}{content from response}
}
\description{
Parse player injuries
}
\examples{
\dontrun{
resp <- player_injuries("nba")
injuries <- parse_player_injuries(resp$content)
}
}
