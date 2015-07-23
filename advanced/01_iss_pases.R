## 01 ISS Passes Visualization
# googlesheets vignette:
# http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html
library(ggplot2)
library(dplyr)
library(lubridate)
library(googlesheets)
suppressMessages(library(dplyr))

## Authenticate
gs_ls()

## Get ISS Passes Sheet
# By public (i.e. published) URL
iss.gs <- gs_url(x = "https://docs.google.com/spreadsheets/d/1POSnldycAhHfrMSG_3SWeUlqlTcXhYj-aB1AuwVzXro/pubhtml")
# By key
iss.gs <- gs_key(x = "1POSnldycAhHfrMSG_3SWeUlqlTcXhYj-aB1AuwVzXro")

## Read ISS Passes from googlesheets object
iss.raw <- gs_read(ss = iss.gs,
               ws = 1)

## Parsing Dates
iss.clean <- iss.raw %>%
  mutate(start = gsub("( at|,)", "", start),
         end = gsub("( at|,)", "", end)) %>%
  mutate(start = parse_date_time(start, "%B %d %Y %I:%M%p"),
         end = parse_date_time(end, "%B %d %Y %I:%M%p")) %>%
  mutate(start.month = month(start, label = TRUE),
         start.yday = yday(start),
         start.mday = mday(start),
         start.wday = wday(start, label = TRUE),
         start.hms = format(start, "%I:%M%p") %>% hm(),
         end.month = month(end, label = TRUE),
         end.yday = yday(end),
         end.mday = mday(end),
         end.wday = wday(end, label = TRUE),
         end.hms = format(end, "%I:%M%p") %>% hm(),
         week = week(start),
         ID = 1:nrow(iss.raw))

names(iss.clean) <- toupper(names(iss.clean))

## Plot
iss.clean %>%
  ggplot(aes(x = START)) +
    geom_bar(aes(y = SECONDS, fill = START.WDAY), stat = "identity") +
    scale_x_datetime(breaks = date_breaks("1 day")) +
    facet_grid(~ START.MONTH, scales = "free", space = "free") +
    coord_flip()

iss.clean %>%
  ggplot(aes(x = START)) +
    geom_bar(aes(y = SECONDS, fill = START.WDAY), stat = "identity") +
    scale_x_datetime(breaks = date_breaks("3 days")) +
    facet_grid(~ START.MONTH, margins = TRUE) +
    coord_flip()