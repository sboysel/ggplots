## using geom_map
library(dplyr)
library(ggplot2)
library(mapproj)
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

us.counties.g.url <- "http://eric.clst.org/wupl/Stuff/gz_2010_us_050_00_500k.json"
us.counties.g <- readOGR(us.counties.g.url, "OGRGeoJSON")
california.counties <- subset(us.counties.g, STATE == "06")

aea.crs <- CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

if (!dir.exists("../data/canada/alberta/")) {
  dir.create("../data")
  dir.create("../data/canada")
  dir.create("../data/canada/alberta")
}

download.file(url = "https://extranet.gov.ab.ca/srd/geodiscover/srd_pub/openData/ProvincialBoundary2010.zip",
              destfile = "../data/canada/alberta/ProvincialBoundary2010.zip",
              method = "libcurl")
unzip(zipfile = "../data/canada/alberta/ProvincialBoundary2010.zip",
      overwrite = TRUE)
canada.url <- "https://raw.githubusercontent.com/mledoze/countries/master/data/can.geo.json"
canada.admin <- readOGR(canada.url, "OGRGeoJSON")

us.counties.df <- prepare_spdf(us.counties.g)
california.counties.df <- prepare_spdf(california.counties)
# california.counties.df <- california.counties.df %>% tbl_df() %>%
#   filter(lat > 0)
canada.admin.df <- prepare_spdf(canada.admin)

ggplot(data = us.counties.df) +
  geom_map(aes(x = long, y = lat, group = group, map_id = id),
           map = us.counties.df) +
  coord_map()

ggplot(data = california.counties.df) +
  geom_map(aes(x = long, y = lat, group = group, map_id = id),
           map = california.counties.df) +
  geom_path(aes(x = long, y = lat, group = group),
            colour = "white") +
  coord_map()

ggplot(data = canada.admin.df) +
  geom_map(aes(x = long, y = lat, group = group, map_id = id),
           map = canada.admin.df) +
  coord_map()

