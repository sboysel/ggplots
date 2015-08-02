## 04: Tide Charts
# http://tidesandcurrents.noaa.gov/api/#requestResponse
library(httr)
library(ggplot2)
library(lubridate)
library(jsonlite)

## Define Functions
get_hm <- function(t) {
  dt <- parse_date_time(t, "%Y-%m-%d %H:%M")
  period <- new_period(hour = hour(dt), minute = minute(dt))
  return(period)
}

get_tides <- function(station,
                      location,
                      product = "high_low",
                      begin = 20130101,
                      end = 20140101,
                      datum = "MTL",
                      units = "english",
                      tz = "gmt",
                      application = "test",
                      format = "json",
                      debug = FALSE) {
  url.base <- "http://tidesandcurrents.noaa.gov/api/datagetter?"
  url.begin.date <- paste0("begin_date=", begin)
  url.end.date <- paste0("&end_date=", end)
  url.station <- paste0("&station=", station)
  url.product <- paste0("&product=", product)
  url.datum <- paste0("&datum=", datum)
  url.units <- paste0("&units=", units)
  url.tz <- paste0("&time_zone=", tz)
  url.application <- paste0("&application=", application)
  url.format <- paste0("&format=", format)
  req <- paste0(url.base, url.begin.date, url.end.date, url.station,
                url.product, url.datum, url.units, url.tz, url.application, url.format)
  if (debug) {
    return(req)
  }
  resp <- httr::GET(req)
  resp.content <- httr::content(resp, as = 'parsed', type = 'application/json')
  df.raw <- do.call(plyr::rbind.fill, lapply(resp.content$data, data.frame, stringsAsFactors = FALSE))
  names(df.raw) <- c("time", "height", "tide_type", "f")
  df.clean <- df.raw %>%
    tbl_df() %>%
    mutate(timestamp = parse_date_time(time, "%Y-%m-%d %H:%M"),
           height = as.numeric(height),
           tide_type = factor(tide_type),
           time = get_hm(time),
           station = station,
           location = location) %>%
    mutate(month = month(timestamp, label = TRUE),
           weekday = wday(timestamp, label = TRUE),
           monthday = mday(timestamp),
           hour = hour(timestamp),
           minute = minute(timestamp),
           day_seconds = period_to_seconds(time)) %>%
    select(-f) %>%
    group_by(monthday) %>%
    mutate(height_hh = max(height),
           height_ll = min(height)) %>%
    ungroup()
  return(df.clean)
}

## Get and clean data
santabarbara <- get_tides(station = 9411340, location = "Santa Barbara, CA")
barharbor <- get_tides(station = 8413320, location = "Bar Harbor, ME")
corpuschristi <- get_tides(station = 8775870, location = "Corpus Christi, TX")
tides <- dplyr::bind_rows(santabarbara, barharbor, corpuschristi)

## Plot
tides %>%
  filter(month == "Aug", monthday <= 10) %>%
  ggplot(aes(x = timestamp, y = height)) +
    geom_line(aes(fill = location, colour = location)) +
    facet_grid(month ~ .)

