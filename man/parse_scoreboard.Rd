% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by-date-parsers.r
\name{parse_scoreboard}
\alias{parse_scoreboard}
\title{Parse scoreboard}
\usage{
parse_scoreboard(json)
}
\arguments{
\item{json}{content from response}
}
\description{
Parse scoreboard
}
\examples{
\dontrun{
resp <- scoreboard("mlb", date = as.Date("2017-07-24"), season = "2017-regular")
parse_scoreboard(resp$content)
}
}
