## LOad libraries:
library(RODBC)
library(sp)
library(rgdal)
library(PBSmapping)

## scripts used
source("R/JoinConditionWithMainQueries.R")
source("R/CreateAccidentsShp.R")
source("R/CalculateDensityPerNeighbourhood.R")


################################################################################################################
### Condition statement###
Sceranio1 <- ""
Sceranio2 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND Uur = '8'"
Sceranio3 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND [a].[Dag_code] != 'WO' AND Uur = '14'"
Sceranio4 <-"WHERE [a].[Dag_code] = 'WO' AND Uur = '12'"
Sceranio5 <-"WHERE [a].[Antl_dod] != '0'"

################################################################################################################
############# USER INPUT #######################################################################################
condition <- Sceranio1
SHPname <- "accidentsScenatio1"
################################################################################################################

## Main qeries (acidents, cbs, victims, parties)
SQLaccidents <- "SELECT Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, Antl_dod, 
    Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, Niveaukop, Wse_id, Maxsnelhd, Wvg_id, Wgd_code_1, 
    X_COORD, Y_COORD, JAARl
    FROM accidents AS [a]
    INNER JOIN [locations] AS [l] ON [a].[FK_veld5] = [l].[FK_VELD5l] AND [a].[Jaar_vkl] = [l].[JAARl]"

SQLcbs <- "SELECT Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, Antl_dod, 
    Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, Niveaukop, Wse_id, Maxsnelhd, Wvg_id, Wgd_code_1, 
    X_COORD, Y_COORD, JAARl, AANT_INW, AANT_VROUW, AANT_VROUW, P_00_14_JR, P_15_24_JR, P_25_44_JR, P_45_64_JR, 
    P_65_EO_JR, P_MAROKKO, P_ANT_ARU, P_SURINAM, P_TURKIJE, AUTO_TOT, AUTO_LAND, MOTOR_2W 
    FROM accidents AS [a]
    INNER JOIN [locations] AS [l] ON [a].[FK_veld5] = [l].[FK_VELD5l] AND [a].[Jaar_vkl] = [l].[JAARl]
    INNER JOIN [CBS] AS [c] ON [c].[BU_CODE] = [l].[BU_CODE] AND [c].[Jaar] = [l].[JAARl]"

SQLvictims <- "SELECT Gebdat, Geslacht, Aardltsl, Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, 
    Ap5_code, Antl_dod, Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, Niveaukop, Wse_id, Maxsnelhd, 
    Wvg_id, Wgd_code_1 
    FROM accidents AS [a]
    INNER JOIN [victims] AS [v] on [v].[Vkl_nummer] = [a].[Vlk_nummer]"

SQLparties <- "SELECT Ote_id, Leeftijd, Lke_id, Ntt_code_b, Geslacht, Blaastest, Art8, Dag_code, Mnd_nummer, Jaar_vkl, 
    Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, Antl_dod, Antl_sla, Antl_gzh, Antl_seh, Antl_gov, Antl_ptj, Mne_code, 
    Niveaukop, Wse_id, Maxsnelhd, Wvg_id, Wgd_code_1 
    FROM accidents AS [a]
    INNER JOIN [parties] AS [p] ON [p].[Vkl_nummer] = [a].[Vlk_nummer]"

## Merge main queries with chosen scenario
AccidentsQ <- JoinCondition(SQLaccidents, condition)
CbsQ <- JoinCondition(SQLcbs, condition)
VictimsQ <- JoinCondition(SQLvictims, condition)
PartiesQ <- JoinCondition(SQLparties, condition)

## Connect to SQL Server Database
connect <- odbcConnect("Data")

## Derive tables
Accidents <- sqlQuery(connect, AccidentsQ)
CBS <- sqlQuery(connect, CbsQ)
Victims <- sqlQuery(connect, VictimsQ)
Parties <- sqlQuery(connect, PartiesQ)

## Derive attributes from accidend table
hour <- Accidents$Uur
time <- Accidents$Tijdstip
day <- Accidents$Dag_code
month <- Accidents$Mnd_nummer
year <- Accidents$Jaar_vkl
severity3 <- Accidents$Ap3_code
severity4 <- Accidents$Ap4_code
severity5 <- Accidents$Ap5_code
casualties <- Accidents$Antl_sla
fatalities <- Accidents$Antl_dod
injuries <- Accidents$Antl_gzh
emergencyVictims <- Accidents$Antl_seh
otherInjured <- Accidents$Antl_gov
parties <- Accidents$Antl_ptj
maneuver <- Accidents$Mne_code
AccNature <- Accidents$Aol_id
networkLevel <- Accidents$Niveaukop
networkLevel2 <- Accidents$Wse_id
maxspeed <- Accidents$Maxsnelhd
weather <- Accidents$Wgd_code_1

## Bind attributes
data <- cbind(hour, time, day, month, year, severity3, severity4, severity5, casualties, fatalities, injuries, 
              emergencyVictims, otherInjured, parties, maneuver, AccNature, networkLevel, networkLevel2, maxspeed, weather)

## Create attributes data frame
mydata <- data.frame(data)

## Bind coordinates
x <- Accidents$X_COORD
y <- Accidents$Y_COORD
coords <- cbind(x,y)

## Create accidents shapefie with name secified by user (in "USER INPUT") and save it in "Output" folder
CreateSHP(mydata, coords, SHPname)











accidentspoints <- readShapePoints("Output/accidentsScenatio1.shp")
plot(accidentspoints, col = "steelblue")
title("Accidetns in Amsterdam")












## Pearson's correlation
cor(mydata, use="complete.obs", method = "pearson")
library(spatialEco)
new_shape <- point.in.poly(mypoints, grid)
AggregationList <- list.files("Data/AggregationLevels", pattern = '.shp')
print(AggregationList)










## Get rid od NA rows
accidentsTime <-Accidents$Uur[!is.na(accidents$Uur)]
accidentsTime <-as.numeric(Accidents$Uur)
den <- density.default(x=accidentsTime)
plot(den, main = "Number of accidents per day time", xlab="time", ylab="number of accidents", type="l", xlim = c(0,23))
axis(1, at=0:23)
polygon(den,col="#821122", border="#cccccc")

barplot()
