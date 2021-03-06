library(sp)
library(rgdal)
library(shapefiles)

Density <- function(SHPfilename, ScenarioName, OutputDirectory) {
  ## RD New projection
  prj_string_RD <-CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 
                       +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,
                      4.0812 +units=m +no_defs")
  
  ## Create a name of the table for current aggregation level (e.g. BuurtenPointDensity)
  tablename <- gsub(".shp","", SHPfilename)
  tablename <- paste(tablename,"PointDensity")
  tablename <- gsub(" ","", tablename)
  print(tablename)
  
  ## Load aggregation level shp
  dsn <- file.path("Data/AggregationLevels", SHPfilename)
  #ogrListLayers(dsn) ## to find out what the layers are
  neigh <- readShapePoly(dsn)
  proj4string(neigh) <- prj_string_RD
  
  ## Create matrix containing all aggregation level ID's
  ID <- neigh$ID
  ID <- as.matrix(ID)
  
  ## Create data frame (neighTab) containing area varibles (total area, water area, etc.)
  neighTab <- data.frame()
  neighTab <- ID
  colnames(neighTab)[1] <- "ID"
  neighTab <- merge(neighTab,neigh,by.x= "ID", by.y = "ID", all = TRUE)
  
  ## Load accidents
  accDir <- paste("accidents", ScenarioName, ".shp")
  accDir <- gsub(" ", "", accDir)
  OutDirAcc <- paste(OutputDirectory, "/AccidentsSHP")
  OutDirAcc <- gsub("/FactorDensity", "", OutDirAcc)
  OutDirAcc <- gsub(" ", "", OutDirAcc)
  dsn3 <- file.path(OutDirAcc, accDir)
  ogrListLayers(dsn3)
  acci <- readShapePoints(dsn3)
  proj4string(acci) <- prj_string_RD
  
  ## Calculate accident density per aggregation level
  aggre <- over(acci, neigh)
  count <- table(aggre$ID, exclude = NULL)
  count <- data.frame(count)
  frequency <- count$Freq
  neighID <- count$Var1
  accidentsTab <- cbind(frequency, neighID)
  accidentsTab <- as.matrix(accidentsTab)
  
  ## Mere accident density (accidentTab) with neighTab
  df <- merge(neighTab,accidentsTab,by.x= "ID", by.y = "neighID", all = TRUE)
  colnames(df)[19] <- "accidents"
  
  print(df)
  
  ## Create a list of factor files
  FactorList <- list.files("Data/Factors", pattern = '.shp')
  
  
  for (j in 1:length(FactorList)) {    
    
    ## Select [j] factor
    filename <- FactorList[j]
    
    ## Load factor
    dsn2 <- file.path("Data/Factors", filename)
    ogrListLayers(dsn2)
    factor <- readShapePoints(dsn2)
    proj4string(factor) <- prj_string_RD 
    
    ## Count point in polygons
    aggre <- over(factor, neigh)
    count <- table(aggre$ID)
    count <- data.frame(count)
    frequency2 <- count$Freq
    neighID2 <- count$Var1
    
    FreTab <- cbind(neighID2, frequency2)
    FreTab <- as.matrix(FreTab)
    
    ## Add frequency of the j factor to df dataframe
    df <- merge(df,FreTab,by.x= "ID", by.y = "neighID2", all = TRUE)
    
    ## change column names
    factorname <- gsub(".shp", "", filename)
    colnames(df)[j+19] <- factorname
  }
  
  OutputDir <- paste(OutputDirectory, "/", tablename, ScenarioName, ".csv")
  OutputDir <- gsub(" ", "", OutputDir)
  print(OutputDir)
  finaldf <- df[-nrow(df),]
  write.table(finaldf, OutputDir, sep = ';', na = '0', dec = ',', col.names = TRUE, row.names = FALSE)
  
  filename <- gsub(".shp", "", SHPfilename)
  filename <- paste(filename, ScenarioName)
  filename <- gsub(" ", "", filename)
  neighDens <- merge(neigh, finaldf, key = "ID")
  writeOGR(neighDens, dsn = OutputDirectory, filename , driver = "ESRI Shapefile")
}