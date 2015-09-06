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

letters <- read.csv("ritaApogeesCleaned.csv")

words <- letters %>% group_by(word, wordBegin, wordEnd, wordList, repetition, wordInstanceID, wordtype, length, buttontype, signer, speed, segment, wordInstanceVideoPath, holdProp) %>% summarise()

words$videoName <- gsub("foo/", "", as.character(words$wordInstanceVideoPath))

answers <- as.character(words$word)
names(answers) <- words$videoName

wordResponses$correct <- {wordResponses$word == answers[gsub(".*stim([[:digit:]]+.mp4)", "\\1",  wordResponses$video)]}

wordResponses %>% group_by(partsessionid, block) %>% summarise(rate = mean(correct))