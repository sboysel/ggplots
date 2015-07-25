## Spatial Plots
library(dplyr)
# library(extrafont)
# library(gpclib)
library(ggplot2)
library(maptools)
library(readr)
library(rgdal)
library(rgeos)
library(rvest)
library(sp)

## Define utility functions
# (1) prepare SpatialPolygonsDataFrame for plotting
# based on this article:
# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
# You may need to install gpclib from CRAN
prepare_spdf <- function(spdf) {
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region = "id")
  if (sum(is.na(spdf.points$hole))) {
    spdf.points <- spdf.points %>%
      filter(hole == FALSE)
  }
  spdf.df <- dplyr::left_join(spdf.points, spdf@data, by = "id")
  return(spdf.df)
}

## Load Data
# (1) California Congressional Boundaries 108 - 112
calif.cb.url <- "https://raw.githubusercontent.com/sboysel/congressional-district-boundaries/master/California_108_to_112.geojson"
calif.cb <- readOGR(calif.cb.url, "OGRGeoJSON")

# Get centroids using spcoordinates
calif.cb.centroids <- coordinates(calif.cb)
calif.cb.data <- data.frame(district = rownames(calif.cb.centroids),
                            long = calif.cb.centroids[, 1],
                            lat = calif.cb.centroids[, 2])
calif.cb.centroids.spdf <- SpatialPointsDataFrame(coords = calif.cb.centroids,
                                                  data = calif.cb.data)
# Get centroids using rgeos::gCentroid
# calif.cb.centroids <- gCentroid(calif.cb, byid = TRUE)
# calif.cb.data <- data.frame(district = rownames(calif.cb.centroids@coords),
#                             long = calif.cb.centroids@coords[, 1],
#                             lat = calif.cb.centroids@coords[, 2])

# (2) Scrape Largest California Cities from Wikipedia
calif.list <- read_html("https://en.wikipedia.org/wiki/List_of_largest_California_cities_by_population") %>%
  html_table(fill = TRUE)

calif.pop <- calif.list[[1]] %>%
  tbl_df() %>%
  select(City, Population) %>%
  mutate(Population = gsub(",", "", Population)) %>%
  mutate(Population = as.numeric(Population))
names(calif.pop) <- c("name", "pop")

# (3) US City Centroids
# http://opengeocode.org/download.php#statecity
if (!file.exists("../data/statecity.zip")) {
  download.file(url = "http://opengeocode.org/download/statecity.zip",
                destfile = "data/statecity.zip")
  unzip(zipfile = "data/statecity.zip",
        exdir = "data/")
}

statecity <- readr::read_csv(file = "data/statecity.csv")
names(statecity) <- c("iso", "fips.state", "name", "fips.city", "gnis", "type", "lat", "long")
s <- statecity %>%
  filter(type == "PPL", iso == "CA") %>%
  select(name, long, lat)
# (4): Join California Cities by Pop (2) with City Centroids (3)
calif.pop.locs <- dplyr::inner_join(calif.pop, s) %>%
  top_n(30, pop)

# (5): Prepare Congressional Districts Polygons for plotting
calif.cb.df <- prepare_spdf(calif.cb)

# Filled by district
# Given there are so many polygons, the choice to fill by district is more for
# decorative purposes than for efficacy.
library(RColorBrewer)
colourCount <- length(unique(calif.cb.df$district))
getPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

cairo_pdf()
ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = district),
            data = calif.cb.df) +
  geom_path(aes(x = long, y = lat, group = group),
            data = calif.cb.df,
            colour = "white",
            size = 0.5) +
  scale_fill_manual(values = getPalette(colourCount)) +
  labs(title = "California Congressional Districts: 108th - 112th") +
  theme(legend.position = "bottom",
        text = element_text(family = "Droid Sans")) +
  guides(fill = guide_legend(nrow = 5))+
  ggsave(filename = "ca-cb-108-112.pdf",
         device = cairo_pdf,
         height = 10,
         width = 8)
dev.off()

# With Congressional District Labels
# Note that district labels may be slightly off or even outside of the polygon in
# some cases.  Using a centroid function is simply easier than placing labels by
# hand.
ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "gray",
               data = calif.cb.df) +
  geom_path(aes(x = long, y = lat, group = group),
            data = calif.cb.df,
            colour = "white",
            size = 0.5) +
  geom_text(aes(x = long, y = lat, label = district),
            data = calif.cb.centroids.spdf@data,
            family = "Droid Sans",
            size = 2) +
  labs(title = "California Congressional Districts: 108th - 112th") +
  theme(legend.position = "none",
        text = element_text(family = "Droid Sans"))

# With Large City Bubbles
ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "gray",
               data = calif.cb.df) +
  geom_path(aes(x = long, y = lat, group = group),
            data = calif.cb.df,
            colour = "white",
            size = 0.5) +
  geom_point(aes(x = long, y = lat, size = pop),
            data = calif.pop.locs,
            pch = 21,
            colour = "black",
            fill = "cornflowerblue",
            alpha = 0.75) +
  scale_size_continuous(labels = scales::comma) +
  labs(title = "Top 30 Most Populous Cities") +
  theme(text = element_text(family = "Droid Sans"))
