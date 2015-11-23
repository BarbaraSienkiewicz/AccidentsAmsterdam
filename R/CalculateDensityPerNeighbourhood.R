library(sp)
library(rgdal)
library(shapefiles)
library(maptools)


Density <- function(SHPfilename) {
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
  proj4string(neigh) <- prj_string_RD
  
  ## Create matrix containing all aggregation level ID's
  ID <- neigh$ID
  ID <- as.matrix(ID)
  colnames(ID)[1]<-"ID"
  
  ## Create empty data frame and fil it with ID column
  df <- data.frame()
  df <- ID
  
  ## Load accidents
  dsn3 <- file.path("Output", "accidentsScenatio1.shp")
  ogrListLayers(dsn3) ## to find out what the layers are
  acci <- readShapePoints(dsn3)
  proj4string(acci) <- prj_string_RD 
  
  ## Check CRS
  proj4string(neigh) 
  proj4string(acci)
  #identicalCRS(accidentsSpa,neigh)
  
  aggre <- over(acci, neigh)
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
  
  
  for (j in 1:length(FactorList)) {    
    ## Select [j] factor
    filename <- FactorList[j]
    
    ## Load factor
    dsn2 <- file.path("Data/Factors", filename)
    ogrListLayers(dsn2)
    factor <- readShapePoints(dsn2)
    proj4string(factor) <- prj_string_RD 
    
    # Check classes
    class(neigh)
    class(factorsSpat)
    
    ## Check CRS
    proj4string(neigh) 
    proj4string(factor)
    
    ## Count point in polygons
    aggre <- over(factor, neigh)
    count <- table(aggre$ID)
    count <- data.frame(count)
    frequency2 <- count$Freq
    neighID2 <- count$Var1
    
    FreTab <- cbind(neighID2, frequency2)
    FreTab <- as.matrix(FreTab)
    
    #merged <- merge(df,factorTab,by.x= "ID", by.y = "neighID2", all = TRUE)
    #factorDens <- merged[3]
    
    df <- merge(df,FreTab,by.x= "ID", by.y = "neighID2", all = TRUE)
    
    factorname <- gsub(".shp", "", filename)
    colnames(df)[j+2] <- factorname
  }
  
  OutputDir <- paste("Output/", tablename, ".csv")
  OutputDir <- gsub(" ", "", OutputDir)
  print(OutputDir)
  finaldf <- df[-nrow(df),]
  finaldf[is.na(finaldf)] <- 0
  output <- write.csv(finaldf, OutputDir)
}