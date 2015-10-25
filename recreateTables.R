source("dbUtils.R")

# # (Re-)create the DB tables table
if(FALSE){
  db <- dbConnect(RMySQL::MySQL(), group = "fsExpAdmin", default.file="./mysql.cnf")
  query <- "DROP TABLE wordResp, participantsession, captchASL"
  results <- dbGetQuery(db, query)
  print(results)
  
  query <- sqlFromFile("participantsessionCreate.sql")
  results <- dbGetQuery(db, query)
  print(results)
  
  query <- sqlFromFile("wordRespCreate.sql")
  results <- dbGetQuery(db, query)
  print(results)
  
  
  query <- sqlFromFile("captchASL.sql")
  results <- dbGetQuery(db, query)
  print(results)
  
  dbDisconnect(db)
}