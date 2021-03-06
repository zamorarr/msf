% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-parsers.r
\name{parse_players}
\alias{parse_players}
\title{Parse player data from json}
\usage{
parse_players(json)
}
\arguments{
\item{json}{list of json data}
}
\description{
Across most MySportsFeeds data there is usually a common format for player data.
This usually includes the player id, first and last names, and position. This function
simply pulls out these fields for you so you don't have to copy code.
}
\keyword{internal}
