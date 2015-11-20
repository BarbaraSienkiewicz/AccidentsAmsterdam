library(sp)
library(rgdal)

CreateSHP <- function(data, coordinates, filename) {
  ## Setting RD New projection
  prj_string_RD <-CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 
                      +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  mypoints <- SpatialPoints(coordinates, proj4string=prj_string_RD)

  mypointsdf <- SpatialPointsDataFrame(
  coordinates, data = data,
  proj4string=prj_string_RD)

  writeOGR(mypointsdf, dsn = 'Output', filename , driver = "ESRI Shapefile")
}

