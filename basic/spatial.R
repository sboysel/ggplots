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

## Load Data
# (1) Canada Borders Vector Layer (GeoJSON)
canada.url <- "https://raw.githubusercontent.com/mledoze/countries/master/data/can.geo.json"
canada.admin <- readOGR(canada.url, "OGRGeoJSON")
canada.admin.aea <- spTransform(canada.admin, aea.crs)

# (2) USA Counties Vector Layer (Topo)
us.counties.url <- "https://raw.githubusercontent.com/mbostock/topojson/master/examples/us-10m.json"
us.counties <- readOGR(us.counties.url, layer = "counties")

# (3) California Congressional Boundaries 108 - 112
calif.cb.url <- "https://raw.githubusercontent.com/sboysel/congressional-district-boundaries/master/California_108_to_112.geojson"
calif.cb <- readOGR(calif.cb.url, "OGRGeoJSON")

# (4) USA Counties Vector Layer (GeoJSON)
# http://eric.clst.org/Stuff/USGeoJSON
# http://eric.clst.org/wupl/Stuff/gz_2010_us_050_00_500k.json
us.counties.g.url <- "http://eric.clst.org/wupl/Stuff/gz_2010_us_050_00_500k.json"
us.counties.g <- readOGR(us.counties.g.url, "OGRGeoJSON")
california.counties <- subset(us.counties.g, STATE == "06")

## Plot
# (1) Using base plotting methods for Spatial objects
plot(canada.admin)
plot(canada.admin.aea)

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

# (4)
california.counties.df <- prepare_spdf(california.counties)
ggplot(data = california.counties.df) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = id)) +
  geom_path(mapping = aes(x = long, y = lat, group = group),
            colour = "white",
            size = 0.5) +
  theme(legend.position = "none")

sb.county.df <- prepare_spdf(subset(california.counties, NAME == "Santa Barbara"))
ggplot(data = sb.county.df) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               fill = "cornflowerblue") +
  geom_path(mapping = aes(x = long, y = lat, group = group),
            colour = "white",
            size = 0.5) +
  theme(legend.position = "none")