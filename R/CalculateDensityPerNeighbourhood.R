library(sp)

AggregationSHPdir <- "Data/AggregationLevels/Burten.shp"
SHPfilename <- "Burten.shp"

i <-1
Density <- function(SHPfilename, AggregationSHPdir) {
  FactorList <- list.files("Data/Factors", pattern = '.shp')
  print(FactorList)
  
  tablename <- gsub(".shp","", SHPfilename)
  tablename <- paste(tablename,"PointDensity")
  tablename <- gsub(" ","", tablename)
  print(tablename)
  
  dsn <- file.path("Data/AggregationLevels", SHPfilename)
  ogrListLayers(dsn) ## to find out what the layers are
  neigh <- readOGR(dsn, layer = ogrListLayers(dsn))
  ##neigh <- spTransform(neigh, proj4string = prj_string_RD)
  
  proj4string(neigh) <- prj_string_RD
  
  
  ##grid <- SpatialPolygons(neigh, proj4string = prj_string_RD)
  
  neighID <- data.frame(neigh)
  df <- neighID$BUURT_ID

  
  for (i in 1:FactorList) {
    filename <- FactorList[i]
    fileDir <- paste("Data/Factors/",filename)
    fileDir <- gsub("\n","",fileDir)
    fileDir <- gsub(" ","",fileDir)
    factors <- importShapefile(fileDir)
    
    ## RD New projection
    prj_string_RD <- CRS("+init=epsg:28992 +proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel
                         +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs")
    x <- factors$X
    y <- factors$Y
    coord <- cbind(x,y)

    factorsSpat <- SpatialPoints(coord, proj4string=prj_string_RD)
    

    
    # Check classes
    class(neigh)
    class(factorsSpat)
    
    ## Check CRS
    proj4string(neigh) 
    proj4string(factorsSpat)
    identicalCRS(factorsSpat,neigh)
    
    ## Count point in polygons
    aggre <- over(factorsSpat, neigh)
    count <- table(aggre$BUURT_NAAM)
    count <- data.frame(count)
    frequency <- count$Freq
    neighbourhood <- count$Var1
    
    df <- cbind(df,frequency)
    }
}

 









