library(RSQLite)

sqlitePath <- file.path("responses", "data.sqlite")

# # (Re-)create the wordResp table
# db <- dbConnect(SQLite(), sqlitePath)
# query <- "DROP TABLE wordResp"
# dbGetQuery(db, query)
# query <- "CREATE TABLE wordResp(partsessionid INTEGER, word TEXT, timestamp TEXT, video TEXT, numInBlock INTEGER, block TEXT, gAnalyticsID TEXT, repetitions INTEGER)"
# dbGetQuery(db, query)
# dbDisconnect(db)

# # (Re-)create the participantsession table
# db <- dbConnect(SQLite(), sqlitePath)
# query <- "DROP TABLE participantsession"
# dbGetQuery(db, query)
# query <- "CREATE TABLE participantsession(id INTEGER PRIMARY KEY, studyCode TEXT, hearingStatus TEXT, ageAcqASL INTEGER, age INTEGER, majorReq TEXT, whyASL TEXT, major TEXT, nativeLang TEXT, ageAcqEng INTEGER, langs TEXT, gAnalyticsID TEXT, startTime TEXT)"
# dbGetQuery(db, query)
# dbDisconnect(db)

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

