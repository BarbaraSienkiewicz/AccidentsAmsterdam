## LOad libraries:
library(RODBC)
library(sp)
library(rgdal)
library(PBSmapping)
library(maptools)
library(plotrix)
library(ggplot2)
library(plyr)

## scripts used
source("R/JoinConditionWithMainQueries.R")
source("R/CreateAccidentsShp.R")
source("R/CalculateDensityPerNeighbourhood.R")
source("R/MultivariateRegression.R")

################################################################################################################
### WHERE statements###
Scenario1 <- ""
Scenario2 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND Uur = '8'"
Scenario3 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND [a].[Dag_code] != 'WO' AND Uur = '15'"
Scenario4 <-"WHERE [a].[Dag_code] = 'WO' AND Uur = '12'"
Scenario5 <-"WHERE [a].[Antl_dod] != '0'"

################################################################################################################
############# USER INPUT #######################################################################################
## Select scenario or write your own WHERE statement
condition <- Scenario5
## Define text which will be added to files name to diffrientiate scenarios
ScenarioName <- "Scenario5"
################################################################################################################
## Create output directory
OutDir <- paste("Output/",ScenarioName)
OutDir <- gsub(" ", "", OutDir)
dir.create(OutDir)

## Main qeries (acidents, cbs, victims, parties)
SQLaccidents <- "SELECT Vlk_nummer, Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, Antl_dod, 
    Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, Niveaukop, Wse_id, Maxsnelhd, Wvg_id, Wgd_code_1, 
    X_COORD, Y_COORD, JAARl
    FROM accidents AS [a]
    INNER JOIN [locations] AS [l] ON [a].[FK_veld5] = [l].[FK_VELD5l] AND [a].[Jaar_vkl] = [l].[JAARl]"

SQLcbs <- "SELECT Vlk_nummer, Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, Antl_dod, 
    Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, Niveaukop, Wse_id, Maxsnelhd, Wgd_code_1, 
    X_COORD, Y_COORD, JAARl, AANT_INW, AANT_VROUW, AANT_VROUW, P_00_14_JR, P_15_24_JR, P_25_44_JR, P_45_64_JR, 
    P_65_EO_JR, P_MAROKKO, P_ANT_ARU, P_SURINAM, P_TURKIJE, AUTO_TOT, AUTO_LAND, MOTOR_2W 
    FROM accidents AS [a]
    INNER JOIN [locations] AS [l] ON [a].[FK_veld5] = [l].[FK_VELD5l] AND [a].[Jaar_vkl] = [l].[JAARl]
    INNER JOIN [CBS] AS [c] ON [c].[BU_CODE] = [l].[BU_CODE] AND [c].[Jaar] = [l].[JAARl]"

SQLvictims <- "SELECT Vlk_nummer, Gebdat, Geslacht, Aardltsl, Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, 
    Ap5_code, Antl_dod, Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, Niveaukop, Wse_id, Maxsnelhd, 
    Wgd_code_1 
    FROM accidents AS [a]
    INNER JOIN [victims] AS [v] on [v].[Vkl_nummer] = [a].[Vlk_nummer]"

SQLparties <- "SELECT Vkl_nummer, Ote_id, Leeftijd, Lke_id, Ntt_code_b, Geslacht, Blaastest, Art8, Dag_code, Mnd_nummer, Jaar_vkl, 
    Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, Antl_dod, Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, 
    Niveaukop, Wse_id, Maxsnelhd, Wgd_code_1 
    FROM accidents AS [a]
    INNER JOIN [parties] AS [p] ON [p].[Vkl_nummer] = [a].[Vlk_nummer]"

## Merge main queries with chosen scenario
AccidentsQ <- JoinCondition(SQLaccidents, condition)
CbsQ <- JoinCondition(SQLcbs, condition)
VictimsQ <- JoinCondition(SQLvictims, condition)
PartiesQ <- JoinCondition(SQLparties, condition)

## Connect to SQL Server Database
connect <- odbcConnect("Data")

## Derive tables by using created queries
Accidents <- sqlQuery(connect, AccidentsQ)
CBS <- sqlQuery(connect, CbsQ)
Victims <- sqlQuery(connect, VictimsQ)
Parties <- sqlQuery(connect, PartiesQ)

################################################################################################################
############# ACCIDENTS ########################################################################################
################################################################################################################
## Derive attributes from accident table
vlk <- Accidents$Vlk_nummer
x <- Accidents$X_COORD
y <- Accidents$Y_COORD
hour <- Accidents$Uur
time <- Accidents$Tijdstip
day <- Accidents$Dag_code
month <- Accidents$Mnd_nummer
year <- Accidents$Jaar_vkl
sev3 <- Accidents$Ap3_code
sev4 <- Accidents$Ap4_code
sev5 <- Accidents$Ap5_code
cas <- Accidents$Antl_sla
fat <- Accidents$Antl_dod
inj <- Accidents$Antl_gzh
emVic <- Accidents$Antl_seh
othInj <- Accidents$Antl_gov
part <- Accidents$Antl_ptj
maneu <- Accidents$Mne_code
AccNat <- Accidents$Aol_id
netLev <- Accidents$Niveaukop
netLev2 <- Accidents$Wse_id
maxspee <- Accidents$Maxsnelhd
weather <- Accidents$Wgd_code_1

## Bind attributes
accidents <- cbind(vlk, x, y, hour, time, day, month, year, sev3, sev4, sev5, cas, fat, inj, 
              emVic, othInj, part, maneu, AccNat, netLev, netLev2, maxspee, weather)

## Create attributes data frame
accidents <- data.frame(accidents)

## Bind coordinates
x <- Accidents$X_COORD
y <- Accidents$Y_COORD
coords <- cbind(x,y)

############# ACCIDENTS SHP ########################################################################################
## Create "Accicents" directory
AccDir <- paste(OutDir, "/AccidentsSHP")
AccDir <- gsub(" ", "", AccDir)
dir.create(AccDir)

## Create accidents shapefie with name secified by user (in "USER INPUT") and save it in "Output" folder
SHPname2 <- paste("accidents", ScenarioName, ".shp")
SHPname2 <- gsub(" ", "", SHPname2)
CreateSHP(accidents, coords, SHPname2, AccDir)

############# FACTORS & ACCIDENTS DENSITY ########################################################################################
## Create "FactorDensity" directory
FacDir <- paste(OutDir, "/FactorDensity")
FacDir <- gsub(" ", "", FacDir)
dir.create(FacDir)

# Create list of the factor files
AggregationList <- list.files("Data/AggregationLevels", pattern = '.shp')

## Create csv tables and shapefiles with density of factors and accients per aggregation level
for (i in 1:length(AggregationList)) {
  AggrSHPfilename <- AggregationList[i]
  Density(AggrSHPfilename, ScenarioName, FacDir)
}

############# REGRESSION ########################################################################################
## Create Regression directory
RegDir <- paste(OutDir, "/Regression")
RegDir <- gsub(" ", "", RegDir)
dir.create(RegDir)

## List csv files with density of accidents and factors per
table.list <- list.files(FacDir, pattern = ".csv")
print(table.list)

## Conduct regression and save the reports in "Regression" folder
for (i in 1:length(table.list)){
  tableDir <- paste(OutDir, "/FactorDensity/", table.list[i])
  tableDir <- gsub(" ", "", tableDir)
  print(tableDir)
  RegOutputDir <- paste(RegDir,"/Regression", table.list[i], ".txt")
  RegOutputDir <- gsub(".csv", "", RegOutputDir)
  RegOutputDir <- gsub(" ", "", RegOutputDir)
  sink(RegOutputDir)
  RegressionAnalysis(tableDir = tableDir)
  sink()
}

############# PLOTS ########################################################################################
## Create Regression directory
PloDir <- paste(OutDir, "/Plots")
PloDir <- gsub(" ", "", PloDir)
dir.create(PloDir)

## Plot number of accidents per hour with severity3
hourDir <- paste(PloDir, "/HoursPlot.png")
hourDir <- gsub(" ", "", hourDir)
accidentsNOnulls <- data.frame(accidents$hour[accidents$hour != "NULL"])
png(filename=hourDir, width = 1800, height = 800, units = "px")
a <- qplot(factor(hour), data=accidentsNOnulls, geom="bar", fill=factor(sev3), main="Number of accidents per hour with their severity") + labs(fill = "Severity") + scale_fill_hue()
b <- a + scale_fill_manual(values = c("#990066", "#9999CC", "#66CC99"), labels = c("Fatalities", "Injuries", "Casualities"))
c <- b + xlab("Hour") + ylab("Accident count") + xlim("0", "1", "2", "3", "4", "5", "6", "7","8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18" , "19", "20", "21", "22", "23")
c
dev.off()

## Plot number of accidents per day
DayDir <- paste(PloDir, "/DaysPlot.png") 
DayDir <- gsub(" ", "", DayDir) 
daysplot <- qplot(day,main="Number of accidents per day",ylab = "Number of accidents", xlab = "Day") + xlim("MA","DI","WO","DO","VR","ZA","ZO")
png(filename=DayDir)
plot(daysplot) 
dev.off() 


## Plot number of accidents per road speed limit
SpeedDir <- paste(PloDir, "/SpeedPlot.png")
SpeedDir <- gsub(" ", "", SpeedDir)
SpeedPlot <-as.numeric(as.character(maxspee))
SpeedPlot <- data.frame(SpeedPlot)
SpeedPlottotals <- table(SpeedPlot)
#rownames(SpeedPlottotals) <- c("15", "30",  "50",  "60",  "70",  "80",  "90", "100", "120", "130")
png(filename=SpeedDir)
plo <- barplot(SpeedPlottotals,main="Number Accidents per Road Speed Limits", ylab="Numer of accidents",xlab="Road Speed Limits")
dev.off()

# Pie Chart from data frame with Appended Sample Sizes
Sev5Dir <- paste(PloDir, "/Severity5Plot.png")
Sev5Dir <- gsub(" ", "", Sev5Dir)
mytable <- table(Accidents$Ap5_code)
lblsC <- c("fatalities", "casualities", "injuries", "emergency casualties", "other injuries")
lblsC <- paste(lblsC, "\n", mytable, sep="")
png(filename=Sev5Dir)
pie(mytable, labels = lblsC, main="Pie Chart of Accidents Severity", col = c("#7B68EE", "#FFD700", "#48D1CC", "#C71585", "#E5E5E5")) 
dev.off()

# Pie Chart from data frame with Appended Sample Sizes
Sev4Dir <- paste(PloDir, "/Severity4Plot.png")
Sev4Dir <- gsub(" ", "", Sev4Dir)
mytable <- table(Accidents$Ap4_code)
lblsC <- c("fatalities", "serious injuries", "light injuries", "material damage")
lblsC <- paste(lblsC, "\n", mytable, sep="")
png(filename=Sev4Dir)
pie(mytable, labels = lblsC, main="Pie Chart of Accidents Severity", col = c("#7B68EE", "#FFD700", "#48D1CC", "#E5E5E5")) 
dev.off()

# Pie Chart from data frame with Appended Sample Sizes
Sev3Dir <- paste(PloDir, "/Severity3Plot.png")
Sev3Dir <- gsub(" ", "", Sev3Dir)
mytable <- table(Accidents$Ap3_code)
lblsC <- c("fatalities", "injuries","material damage")
lblsC <- paste(lblsC, "\n", mytable, sep="")
png(filename=Sev3Dir)
pie(mytable, labels = lblsC, main="Pie Chart of Accidents Severity", col = c("#7B68EE", "#48D1CC", "#E5E5E5"))
dev.off()

################################################################################################################
############# CBS ##############################################################################################
################################################################################################################
## Derive attributes from CBS table
vlk <- CBS$Vlk_nummer
dayC <- CBS$Dag_code 
monthC <- CBS$Mnd_nummer 
yearC <- CBS$Jaar_vkl
hourC <- CBS$Uur
timeC <-CBS$Tijdstip
sev3C <- CBS$Ap3_code
sev4C <- CBS$Ap4_code
sev5C <- CBS$Ap5_code
fatC <- CBS$Antl_dod
casC <-CBS$Antl_sla
injC <- CBS$Antl_gzh
emVicC <-CBS$Antl_seh
othInjC <- CBS$Antl_gov
partC <- CBS$Antl_ptj
manC <- CBS$Mne_code
netLevC <- CBS$Niveaukop
netLev2C <- CBS$Wse_id
maxspC <- CBS$Maxsnelhd
weathC <- CBS$Wgd_code_1
xC<- CBS$X_COORD
yC <- CBS$Y_COORD
popC <- CBS$AANT_INW
womC<- CBS$AANT_VROUW
menC<- CBS$AANT_VROUW
a0_14 <- CBS$P_00_14_JR
a15_24 <- CBS$P_15_24_JR
a25_44 <- CBS$P_25_44_JR
a45_64<- CBS$P_45_64_JR
a65<- CBS$P_65_EO_JR
maroc <- CBS$P_MAROKKO
NLIsl<- CBS$P_ANT_ARU
suri <- CBS$P_SURINAM
turk <- CBS$P_TURKIJE
carTo <- CBS$AUTO_TOT
carLa <- CBS$AUTO_LAND
mot <- CBS$MOTOR_2W
################################################################################################################
#cbs <- cbind(vlk, xC, yC, dayC, monthC, yearC, hourC, timeC, severity3C, severity4C, severity5C,fatC, casC, injC, emVicC,
#             othInjC, partC, manC, netLevC, netLev2C, maxspC, weathC, popC, womC, menC, a0_14,
#             a15_24, a25_44, a45_64, a65, maroc, NLIsl, suri, turk, carTo, carLa, mot)

cbs <- data.frame(CBS)

# Change missing values (-99999998) into NA
cbs[cbs == -99999998] <- NA

## Bind coordinates
coordsC <- cbind(xC,yC)

## Create accidents shapefie with name secified by user (in "USER INPUT") and save it in "Output" folder
SHPname3 <- paste("accidentsCBS", ScenarioName, ".shp")
SHPname3 <- gsub(" ", "", SHPname3)
CBSdir <- paste(OutDir, "/AccidentsSHP")
CBSdir <- gsub(" ", "", CBSdir)
CreateSHP(cbs, coordsC, SHPname3, CBSdir)

CreateSHP(accidents, coords, SHPname2, AccDir)
################################################################################################################
############# VICTIMS ##########################################################################################
################################################################################################################
## Derive attributes from Victims table
vlk <- Victims$Vlk_nummer
birthdate <-Victims$Gebdat
gender <-Victims$Geslacht
severityV<-Victims$Aardltsl
dayV <-Victims$Dag_code
monthV <-Victims$Mnd_nummer
yearV <-Victims$Jaar_vkl
hourV <-Victims$Uur
timeV <-Victims$Tijdstip
severity3V <-Victims$Ap3_code
severity4V <-Victims$Ap4_code
severity5V <-Victims$Ap5_code
fatalitiesV<-Victims$Antl_dod
casualtiesV<-Victims$Antl_sla
injuriesV <-Victims$Antl_gzh
emergencyVictimsV <-Victims$Antl_seh
otherInjuredV <-Victims$Antl_gov
partiesV <-Victims$Antl_ptj
maneuverV<-Victims$Mne_code
networkLevelV<-Victims$Niveaukop
networkLevel2V<-Victims$Wse_id
maxspeedV <-Victims$Maxsnelhd
weatherV<-Victims$Wgd_code_1
################################################################################################################
victims <- cbind(vlk, birthdate, gender, severityV, dayV, monthV, yearV, hourV, timeV, severity3V, severity4V, severity5V,
                 fatalitiesV, casualtiesV, injuriesV, emergencyVictimsV, otherInjuredV, partiesV, maneuverV, networkLevelV,
                 networkLevel2V, maxspeedV, weatherV)

victims <- data.frame(victims)

## Plot number of accidents per gender
GenDir <- paste(PloDir, "/GenderPlot.png")
GenDir <- gsub(" ", "", GenDir)
genderNOnulls <- gender[gender != "NULL"]
png(filename=GenDir)
qplot(genderNOnulls, xlab = "Gender", ylab = "Number of accidents")
dev.off()

## Calculate age of the victims
birthyear <- c()
for (i in 1:length(birthdate)) {
  birthyear[i] <- substr(birthdate[i], 1, 4)
}
birthyearnum <- data.frame(as.numeric(birthyear))

ageTab <- c()
for(i in 1:length(birthyearnum)){
  ageTab[i] <- 2015 - birthyearnum
}
ageTab

AgeDir <- paste(PloDir, "/VictimsAgePlot.png")
AgeDir <- gsub(" ", "", AgeDir)
ageTab2 <- data.frame(ageTab)
ageTabFre <- table(ageTab2)
ageTabFre 
class(ageTabFre)
png(filename=AgeDir,width = 1800, height = 800, units = "px" )
barplot(ageTabFre,main="Number victims per age", ylab="Numer of victims",xlab="Age")
dev.off()


################################################################################################################
############# PARTIES ##########################################################################################
################################################################################################################
## Derive attributes from parties table
vlk <- Parties$Vlk_nummer
partyP<- Parties$Ote_id
ageP <- Parties$Leeftijd
agerangeP<- Parties$Lke_id
nationalityP <- Parties$Ntt_code_b
genderP <- Parties$Geslacht
blowTest <- Parties$Blaastest
art8<- Parties$Art8
dayP <- Parties$Dag_code
monthP <- Parties$Mnd_nummer
yearP <- Parties$Jaar_vkl
hourP <- Parties$Uur
timeP <- Parties$Tijdstip
severity3P<- Parties$Ap3_code
severity4P<- Parties$Ap4_code
severity5P<- Parties$Ap5_code
fatalitiesP <- Parties$Antl_dod
casualtiesP <- Parties$Antl_sla
injuriesP<- Parties$Antl_gzh
emergencyVictimsP <- Parties$Antl_seh
otherInjuredP <- Parties$Antl_gov
partiesP <- Parties$Antl_ptj
maneuverP <- Parties$Mne_code
networkLevelP<- Parties$Niveaukop
networkLevel2P <- Parties$Wse_id
maxspeedP <- Parties$Maxsnelhd
weatherP <- Parties$Wgd_code_1
################################################################################################################
parties <- cbind(vlk, partyP, ageP, agerangeP, nationalityP, genderP, blowTest, art8, dayP, monthP, yearP, hourP, 
                 timeP, severity3P, severity4P, severity5P, fatalitiesP, casualtiesP, injuriesP, emergencyVictimsP, 
                 otherInjuredP, partiesP, maneuverP, networkLevelP, networkLevel2P, maxspeedP, weatherP)

parties <- data.frame(parties)
################################################################################################################
################################################################################################################