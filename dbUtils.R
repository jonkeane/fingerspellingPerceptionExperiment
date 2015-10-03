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
