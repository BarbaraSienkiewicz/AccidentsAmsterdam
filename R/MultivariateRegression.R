RegressionAnalysis <- function(tableDir) {
  
  table <- read.csv(tableDir, header=TRUE, sep=';')
  
  ## Dependent variable
  accidents <-table$accidents
  
  ## Factors
  BasSch <- table$BasScho
  BicPat <- table$BicCross
  Cross <- table$BRcross	
  ChildCare<- table$ChilDay
  HighUni <- table$HighUni	
  alkohol <- table$AlcoRet	
  BikBarr <- table$BikBarr
  stops <- table$BTStops
  OSMcross <- table$OSMcross
  HostHot <- table$HosHot	
  MotJun <- table$MotJun	
  PriorRi<- table$PrioRigh
  Scho <- table$Schools	
  SecScho<- table$SecScho
  SpecScho <- table$SpecScho	
  TraffLig<- table$TraffLi
  
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
                                    OSMcross + HostHot + MotJun + PriorRi + Scho + SpecScho + SecScho + TraffLig)^2)
  print(summary(fitted.model))
  
}
