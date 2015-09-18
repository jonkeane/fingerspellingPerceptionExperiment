library(DBI)
library(RMySQL)

sqlFromFile <- function(file){
  fileconn<-file(file,"r")            
  sqlString<-readLines(fileconn)           
  sqlString<-paste(sqlString,collapse="") 
  return(sqlString)
}

# generic connection
fsdbConn <- function(){dbConnect(RMySQL::MySQL(), group = "fsExpUser", default.file="./mysql.cnf")}

loadData <- function(table) {
  # Connect to the database
  db <- fsdbConn()
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

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