RegressionAnalysis <- function(tableDir) {
  
  table <- read.csv(tableDir, header=TRUE, sep=';')
  
  ## Dependent variable
  accidents <-table$accidents
  
  ## Factors
  BasSch <- table$BasisSchoolsAmsterdam	
  BicPat <- table$BicyclePathsAmsterdamCrossings	
  Cross <- table$BronNetworkCrossings	
  ChildCare<- table$ChildDaycareAmsterdam2011	
  HighUni <- table$HighAndUniAmsterdam2011	
  alkohol <- table$OSMAlcoholBarsNightclubs	
  BikBarr <- table$OSMbikeBarriers2014	
  stops <- table$OSMbusAndTramStops	
  OSMcross <- table$OSMcrossings	
  HostHot <- table$OSMhostelsHotels	
  MotJun <- table$OSMmotorwayJunctions	
  OSMcross2<- table$OSMNetworkAmsterdamCrossings	
  PriorRi<- table$PriorityToRight	
  Scho <- table$SchoolsAmsterdam2011	
  SecScho<- table$SecondarySchoolsAmsterdam2011	
  SpecScho <- table$SpecialSchoolsAmsterdam2011	
  TraffLig<- table$TrafficLightsAmsterdam
  
  ## Output file name
  filename <- gsub("Output/", "", tableDir)
  filename <- gsub(".csv", "", filename)
  filename <- paste(filename, ".txt")
  filename <- gsub("PointDensity", "Regression", filename)
  filename <- gsub(" ", "", filename)
  print(filename)
  
  ## Output directory
  lmFileDir <- paste("Output/Regression/", filename)
  lmFileDir <- gsub(" ", "", lmFileDir)
  print(lmFileDir)
  
  ## Linear model
  fitted.model <- lm(accidents ~ (BasSch + BicPat + Cross + ChildCare + HighUni+ alkohol+ BikBarr + stops + 
                       OSMcross + HostHot + MotJun + OSMcross2 + PriorRi + Scho + SpecScho + SecScho + TraffLig)^2)
  print(summary(fitted.model))

}
