library(sp)
library(rgdal)

CreateSHP <- function(data, coordinates, filename) {
  ## Setting RD New projection
  prj_string_RD <- CRS("+init=epsg:28992 +proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 
                        +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel  +towgs84=565.4171,50.3319,465.5524,
                        -0.398957388243134,0.343987817378283, -1.87740163998045,4.0725 +units=m +no_defs")
  mypoints <- SpatialPoints(coordinates, proj4string=prj_string_RD)

  mypointsdf <- SpatialPointsDataFrame(
  coordinates, data = data,
  proj4string=prj_string_RD)

  writeOGR(mypointsdf, dsn = 'Output', filename , driver = "ESRI Shapefile")
}

