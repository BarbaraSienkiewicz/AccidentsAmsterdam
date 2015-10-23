## LOad libraries:
library(RODBC)

################################################################################################################
############# USER INPUT #######################################################################################
### Condition statement###
Sceranio1 <- ""
Sceranio2 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND Uur = '8'"
Sceranio3 <-"WHERE [a].[Dag_code] != 'ZA' AND [a].[Dag_code] != 'ZO' AND [a].[Dag_code] != 'WO' AND Uur = '14'"
Sceranio4 <-"WHERE [a].[Dag_code] = 'WO' AND Uur = '12'"
Sceranio5 <-"WHERE [a].[Antl_dod] != '0'"

################################################################################################################
############# USER INPUT #######################################################################################
condition <- Scenario2
################################################################################################################


SQLfactors <- "SELECT Dag_code, Mnd_nummer, Jaar_vkl, Uur, Tijdstip, Ap3_code, Ap4_code, Ap5_code, 
 Antl_dod, Antl_sla, Antl_gzh, Niveaukop, X_COORD, Y_COORD, JAARl, PrioRight, TraffLight, SchoolTOT, 
 BasiSchool,SecoSchool, ChilCar, SpecSchool, HighUni, BikeBarr, BikeJun, OSMjun, BRONjun, MosmOpen, 
 MbikeOpen, MbikeOpen
 FROM locations AS [l]
 INNER JOIN [factors] AS [f] ON [f].[BUURT_IDf] = [l].[BUURT_ID]
 INNER JOIN [network] AS [n] ON [n].[BU_CODE] = [l].[BU_CODE]
 INNER JOIN [accidents] AS [a] ON [a].[FK_veld5] = [l].[FK_VELD5l] AND [a].[Jaar_vkl] = [l].[JAARl]"

SQLcbs <- "SELECT X_COORD, Y_COORD, JAAR, AANT_INW, AANT_VROUW, AANT_VROUW, P_00_14_JR, P_15_24_JR, 
 P_25_44_JR, P_45_64_JR, P_65_EO_JR, P_WEST_AL, P_N_W_AL, P_MAROKKO, P_ANT_ARU, P_SURINAM, P_TURKIJE, 
 AUTO_TOT, AUTO_LAND, MOTOR_2W
 FROM locations AS [l]
 INNER JOIN [CBS] AS [c] ON [c].[BU_CODE] = [l].[BU_CODE] AND [c].[Jaar] = [l].[JAARl]
 INNER JOIN [accidents] AS [a] ON [a].[FK_veld5] = [l].[FK_VELD5l] AND [a].[Jaar_vkl] = [l].[JAARl]"

## Join conditions with Query1 and Query2
Query1 <- paste(SQLfactors, condition, sep = " ", collapse = "\n")
Query1 <- gsub("\n", "", Query1)
print(Query1)

Query2 <- paste(SQLcbs, condition, sep = " ", collapse = NULL)
Query2 <- gsub("\n", "", Query2)
print(Query2)



## SQL Server Database Connection
conect <- odbcConnect("Data")



AccFactors <- sqlQuery(conect, Query1)
summary(AccFactors)
x <- AccFactors$X_COORD
AccFactors$X_COORD <-as.numeric(x)
y <-AccFactors$Y_COORD
AccFactors$Y_COORD <- as.numeric(y)

write.csv2(AccFactors, file = "Output/Factors.csv", fileEncoding = "UTF-8")


AccCBS <- sqlQuery(conect, Query2)
summary(AccCBS)




## Get rid od NA rows
accidentsTime <-accidents$Uur[!is.na(accidents$Uur)]


accidentsTime <-as.numeric(accidents$Uur)


den <- density.default(x=accidentsTime)
plot(den, main = "Number of accidents per day time", xlab="time", ylab="number of accidents", type="l", xlim = c(0,23))
axis(1, at=0:23)
polygon(den,col="#821122", border="#cccccc")

barplot()
