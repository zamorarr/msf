[![Travis-CI Build Status](https://travis-ci.org/zamorarr/msf.svg?branch=master)](https://travis-ci.org/zamorarr/msf)

## msf

msf is an R wrapper for the [MySportsFeeds API](https://www.mysportsfeeds.com/).

There is an [official R package](https://github.com/MySportsFeeds/mysportsfeeds-r) provided by MySportsFeeds, but this package differs in the following ways:

- Provides `parse_*` functions to convert a JSON list into a tidy dataframe. 
- Provides `delay` parameter in API calls to execute multiple queries with a delay between them in order to obey rate limits.
- Provides common defaults for some parameters (ex. uses `season = "current"`) to reduce typing.

These additions have been very helpful for me to use the data effectively in R. Also, when I started this package the official R package had not been released yet. 

## Install
```R
devtools::install_github("zamorarr/msf")
```

## Usage
To start, make sure you have set the following environment variables set on your computer. Putting them in your *.Renviron* file is probably the easiest way to do it.

```sh
MYSPORTSFEEDS_USER=YOUR_USERNAME
MYSPORTSFEEDS_PASSWORD=YOUR_PASSWORD
```

Then in R you can query the feeds easily. See the function documentation for a description of the parameters. They should follow the same parameters required to query the official web API.

```R
library(msf)

# Get data
resp <- game_boxscore("nba", "20171027-BRO-NYK")

# You can directly work with the json content represented as a list
json <- resp$content

# Or parse the json list into tidy dataframe
boxscore <- parse_boxscore(json)
```
