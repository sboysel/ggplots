## Spatial Plots
library(dplyr)
# library(gpclib)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
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

# Reproject Canada to Canada Albers Equal Area Projection
aea.crs <- CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
canada.admin.aea <- spTransform(canada.admin, aea.crs)
plot(canada.admin.aea)

## Load Data
# (1) Canada Borders Vector Layer
canada.url <- "https://raw.githubusercontent.com/mledoze/countries/master/data/can.geo.json"
canada.admin <- readOGR(canada.url, "OGRGeoJSON")

## Plot
# (1) Using base plotting methods for Spatial objects
plot(canada.admin)

# (2) Now plot using gglot2
canada.admin.df <- prepare_spdf(canada.admin)
canada.admin.aea.df <- prepare_spdf(canada.admin.aea)
ggplot() +
  geom_polygon(data = canada.admin.df,
               mapping = aes(x = long, y = lat, group = group)) +
  geom_path(data = canada.admin.df,
            mapping = aes(x = long, y = lat, group = group),
            colour = "orange",
            alpha = 0.5)

ggplot() +
  geom_polygon(data = canada.admin.aea.df,
               mapping = aes(x = long, y = lat, group = group)) +
  geom_path(data = canada.admin.aea.df,
            mapping = aes(x = long, y = lat, group = group),
            colour = "orange",
            alpha = 0.5)
