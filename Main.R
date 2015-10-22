
## SQL Server Database Connection
con <- odbcConnect("Data")


accidents <- sqlQuery(con, "SELECT * FROM accidents AS [a] 
                      INNER JOIN [locations] AS [l] ON [l].[FK_veld5] = [a].[FK_veld5] 
                      AND [l].[Jaar] = [a].[Jaar_vkl]")

summary(accidents)
  \

