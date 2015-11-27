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
### Condition statement###
Scenario1 <- ""
Scenario2 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND Uur = '8'"
Scenario3 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND [a].[Dag_code] != 'WO' AND Uur = '14'"
Scenario4 <-"WHERE [a].[Dag_code] = 'WO' AND Uur = '12'"
Scenario5 <-"WHERE [a].[Antl_dod] != '0'"

################################################################################################################
############# USER INPUT #######################################################################################
condition <- Scenario1
SHPname <- "accidentsScenario1.shp"
################################################################################################################

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

## Derive tables
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
################################################################################################################
## Bind attributes
accidents <- cbind(vlk, x, y, hour, time, day, month, year, severity3, severity4, severity5, casualties, fatalities, injuries, 
              emergencyVictims, otherInjured, parties, maneuver, AccNature, networkLevel, networkLevel2, maxspeed, weather)

## Create attributes data frame
accidents <- data.frame(accidents)

## Bind coordinates
x <- Accidents$X_COORD
y <- Accidents$Y_COORD
coords <- cbind(x,y)

## Create accidents shapefie with name secified by user (in "USER INPUT") and save it in "Output" folder
CreateSHP(accidents, coords, SHPname)

# Create list of the factor files
AggregationList <- list.files("Data/AggregationLevels", pattern = '.shp')

## Create csv tbles with density of factors and accients per aggregation level
for (i in 1:length(AggregationList)) {
  AggrSHPfilename <- AggregationList[i]
  Density(AggrSHPfilename, SHPname)
}

table.list <- list.files("Output", pattern = ".csv")
print(table.list)

for (t in 1:length(table.list)) {
  tableDir <- paste("Output/", table.list[t])
  tableDir <- gsub(" ", "", tableDir)
  print(tableDir)
  RegressionAnalysis(tableDir)
}





# Pie chart of severity type
casA <- sum(casualties)
fatA <- sum(fatalities)
injA <- sum(injuries)
emerA <- sum(emergencyVictims)
othA <- sum(otherInjured)
slicesA <- c(fatA, casA, injA, emerA, othA) 
lblsA <- c("fatalities", "casualities", "injuries", "emergency casualties", "other injuries")
pie3D(slicesA,labels=lblsA,explode = 0.1, main="Severity of accidents")

## Plot number of accidents per hour with severity3
accidents <- arrange(accidents, severity3)
toBeRemoved<-which(accidents$hour=="25")
accidentsNOnulls <-accidents[-toBeRemoved,]
a <- ggplot(accidentsNOnulls, aes(x=hour, y=hour, fill = severity3)) + labs(x="Hour" , y="Number of accidents", fill = NULL)
b <- a + geom_bar(stat="identity", position = "stack")
c <- b + scale_x_continuous(breaks=c(1:24))
c

HoursPlot

png(filename="Output/Plots/HoursPlot.png")
plot(HoursPlot)
dev.off()


## Plot number of accidents per day
daysplot <- qplot(day)
png(filename="Output/Plots/DaysPlot.png")
plot(daysplot)
dev.off()

## Plot number of accidents per gender
genderNOnulls <- gender[gender != "NULL"]
qplot(genderNOnulls, xlab = "Gender", ylab = "Number of accidents")
png(filename="Output/Plots/GenderPlot.png")
plot(daysplot)
dev.off()



qplot(severity3)


qplot(severity4)


qplot(severity5)


qplot(maxspeed)


qplot(weather)



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
severity3C <- CBS$Ap3_code
severity4C <- CBS$Ap4_code
severity5C <- CBS$Ap5_code
fatalitiesC <- CBS$Antl_dod
casualtiesC <-CBS$Antl_sla
injuriesC <- CBS$Antl_gzh
emergencyVictimsC <-CBS$Antl_seh
otherInjuredC <- CBS$Antl_gov
partiesC <- CBS$Antl_ptj
maneuverC<- CBS$Mne_code
networkLevelC <- CBS$Niveaukop
networkLevel2C <- CBS$Wse_id
maxspeedC <- CBS$Maxsnelhd
weatherC <- CBS$Wgd_code_1
xC<- CBS$X_COORD
yC <- CBS$Y_COORD
populationC <- CBS$AANT_INW
womenC<- CBS$AANT_VROUW
menC<- CBS$AANT_VROUW
age0_14 <- CBS$P_00_14_JR
age15_24 <- CBS$P_15_24_JR
age25_44 <- CBS$P_25_44_JR
age45_64<- CBS$P_45_64_JR
age65<- CBS$P_65_EO_JR
marocco <- CBS$P_MAROKKO
dutchIslandsC<- CBS$P_ANT_ARU
surinam <- CBS$P_SURINAM
turkey <- CBS$P_TURKIJE
carsTot <- CBS$AUTO_TOT
carsLand <- CBS$AUTO_LAND
motor <- CBS$MOTOR_2W
################################################################################################################
cbs <- cbind(vlk, xC, yC, dayC, monthC, yearC, hourC, timeC, severity3C, severity4C, severity5C,fatalitiesC, casualtiesC, injuriesC, emergencyVictimsC,
                 otherInjuredC, partiesC, maneuverC, networkLevelC, networkLevel2C, maxspeedC, weatherC, populationC, womenC, menC, age0_14,
                 age15_24, age25_44, age45_64, age65, marocco, dutchIslandsC, surinam, turkey, carsTot, carsLand, motor)

cbs <- data.frame(cbs)

# Change missing values (-99999998) into NA
cbs[cbs == -99999998] <- NA

## Bind coordinates
coordsC <- cbind(xC,yC)

## Create accidents shapefie with name secified by user (in "USER INPUT") and save it in "Output" folder
CreateSHP(cbs, coordsC, "accidentsCBS")

# Pie chart of severity type
casA <- sum(casualties)
fatA <- sum(fatalities)
injA <- sum(injuries)
emerA <- sum(emergencyVictims)
othA <- sum(otherInjured)
slicesA <- c(fatA, casA, injA, emerA, othA) 
lblsA <- c("fatalities", "casualities", "injuries", "emergency casualties", "other injuries")
pie3D(slicesA,labels=lblsA,explode = 0.1, main="Severity of accidents")


# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(CBS$Ap3_code)
lblsC <- c("fatalities", "casualities", "injuries", "emergency casualties", "other injuries")
lblsC <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lblsC, 
    main="Pie Chart of Severity") 

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
class(victims)
# Pie chart of severity type
cas <- sum(casualtiesV)
fat <- sum(fatalitiesV)
inj <- sum(injuriesV)
emer <- sum(emergencyVictimsV)
oth <- sum(otherInjuredV)
slices <- c(fat, cas, inj, emer, oth) 
lbls <- c("fatalities", "casualities", "injuries", "emergency casualties", "other injuries")
pie3D(slices,labels=lbls, explode = 0.1, main="Severity of accidents")


frequency <- table(severity3V)

library(plyr)
ce <- arrange(victims, severity3V)
ce <- ddply(ce, "severity3V", transform, label_y=cumsum(severity3V))
ce

## number of accidents per hour
ggplot(victims, aes(x=hourV, y=hourV, fill=severity3V)) + geom_bar(stat="identity", width=0.7) + guides(fill=guide_legend(reverse=TRUE))
+ geom_text(aes(y = label_y, label=frequency), vjust=-0.2)


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
parties <- parties[parties$ageP!=NUL,]

ggplot(parties, aes(ageP)) + geom_histogram()+ scale_x_continuous(breaks=c(1:100))

################################################################################################################
################################################################################################################


library(Hmisc)
M <- cor(accidents)
##rcorr(data, type="pearson")
library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix

## Describe data
describe(victims)


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
