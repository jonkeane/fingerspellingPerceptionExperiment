library(RSQLite)
library(dplyr)

sqlitePath <- file.path("responses", "data.sqlite")

loadData <- function(table) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

participantsessions <- loadData("participantsession")
wordResponses <- loadData("wordResp")

realResp <- filter(wordResponses, partsessionid > 3)
realResp$timestamp <- as.numeric(realResp$timestamp)

print(realResp %>% group_by(partsessionid) %>% summarise(duration = (max(timestamp)-min(timestamp))/60))

