library(sp)
library(rgdal)
library(shapefiles)

Density <- function(SHPfilename, accidentSpatialPoints) {
  
  ## RD New projection
  prj_string_RD <-CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 
                      +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  ## Create a name of the table for current aggregation level (e.g. BuurtenPointDensity)
  tablename <- gsub(".shp","", SHPfilename)
  tablename <- paste(tablename,"PointDensity")
  tablename <- gsub(" ","", tablename)
  print(tablename)
  
  ## Load aggregation level shp
  dsn <- file.path("Data/AggregationLevels", SHPfilename)
  ogrListLayers(dsn) ## to find out what the layers are
  neigh <- readShapePoly(dsn)
    #readOGR(dsn, layer = ogrListLayers(dsn))
  proj4string(neigh) <- prj_string_RD
  
  ## Create matrix containing all aggregation level ID's
  ID <- neigh$ID
  ID <- as.matrix(ID)
  colnames(ID)[1]<-"ID"
  
  ## Create empty data frame and fil it with ID column
  df <- data.frame()
  df <- ID
  
  
  ## Calculate the density of accidents per aggregation level
  accidentsSpa <- accidentSpatialPoints
  proj4string(accidentsSpa) <- prj_string_RD
  
  ## Check CRS
  proj4string(neigh) 
  proj4string(accidentsSpa)
  identicalCRS(accidentsSpa,neigh)
  
  
  aggre <- over(accidentsSpa, neigh)
  count <- table(aggre$ID, exclude = NULL)
  count <- data.frame(count)
  frequency <- count$Freq
  neighID <- count$Var1
  accidentsTab <- cbind(frequency, neighID)
  accidentsTab <- as.matrix(accidentsTab)
  df <- merge(ID,accidentsTab,by.x= "ID", by.y = "neighID", all = TRUE)
  colnames(df)[2] <- "accidents"

  colnames(df)
  print(df)
  
  ## Create a list of factor files
  FactorList <- list.files("Data/Factors", pattern = '.shp')
  print(FactorList)
  
  ## Calculate density of accidents and factors per aggregation level
  for (i in 1:length(FactorList)) {

    ## Select [i] factor
    filename <- FactorList[i]
    
    ## Create [i] factor file directory
    #fileDir <- paste("Data/Factors/",filename)
    #fileDir <- gsub("\n","",fileDir)
    #fileDir <- gsub(" ","",fileDir)
    
    ## Import [i] factor shp file
    dsn2 <- file.path("Data/Factors",filename)
    #ogrListLayers(dsn2)
    factors <- readShapePoints(dsn2)
    #readOGR(dsn2, layer = ogrListLayers(dsn2))
    
    #proj4string(factors) <- prj_string_RD

    ## Select coordinates and create SpatialPoints
    x <- factors$X
    y <- factors$Y
    coord <- cbind(x,y)
    #factorsSpat <- spTransform(factorsSpat, CRS=prj_string_RD)
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
    count <- table(aggre$ID)
    count <- data.frame(count)
    frequency2 <- count$Freq
    neighID2 <- count$Var1
    
    accidentsTab2 <- cbind(frequency2, neighID2)
    accidentsTab2 <- as.matrix(accidentsTab2)
    
    df <- merge(df,accidentsTab2,by.x= "ID", by.y = "neighID2", all = TRUE)
    
    factorname <- gsub(".shp", "", filename)
    colnames(df)[i+2] <- factorname
  }
  
  OutputDir <- paste("Output/", tablename, ".csv")
  OutputDir <- gsub(" ", "", OutputDir)
  print(OutputDir)
  output <- write.csv(df, OutputDir)
}

