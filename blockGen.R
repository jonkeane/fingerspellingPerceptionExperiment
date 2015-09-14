library(jsonlite)
library(dplyr)


blockGen <- function(blockStruct, videosToUse, stimDir, maskColor, aws="", playBackrepetitions=1, transOnlyFirst=FALSE) {
  
  # Concat the directory lsiting for the stimuli (including the aws path)
  stimDir <- paste(aws, stimDir, sep="/")

  # read in the stimuli words, and process them according to least seen.
  videosDF <- read.csv(videosToUse)
  
  wordRespData <- loadData("wordResp")
  partSess <- loadData("participantsession")
  prevData <- merge(wordRespData, partSess, by.x="partsessionid", by.y="id")
  
  prevData$video <- as.factor(prevData$video)
  
  prevData %>% group_by(video) %>% summarise(nResps = length(timestamp)) -> test
  print(test)
#     
#   gAnalyticsID
  
  
  videos <- as.character(videosDF$stimName)
  
  # grab structure and messages from the external json file.
  blocks <- fromJSON(blockStruct)

  # cut the list of videos to use into chunks based on the block descriptions
  chunks <- sapply(blocks, function(list){return(list[["numStims"]])})
  videos <- head(videos, sum(chunks))
  videochunks <- split(videos, rep(1:length(chunks), chunks))
  
  videoInsert <- function(list, videos){
    videoDF <- data.frame(
      num = 1:length(videos),
      stimDir = stimDir,
      speed = list[["speed"]],
      maskColor = list[["maskColor"]],
      maskType = list[["condition"]],
      stim = videos,
      rep = playBackrepetitions,
      stringsAsFactors = FALSE
    )
    
    videoDF$video = with(videoDF, file.path(stimDir, speed, maskColor, maskType, stim))
    
    
    list[["videos"]] <- videoDF
    return(list)
  }

  blocksOut <- mapply(videoInsert, blocks, videochunks, SIMPLIFY = FALSE)
  
  # there ought to be an easier way to do this. Change the json file spec?
  if(transOnlyFirst){
    newBlocks = list("practice" = blocks[["practice"]],
                     "allClearA" = blocks[["allClearA"]],
                     "transitionsOnly" = blocks[["transitionsOnly"]],
                     "holdsOnly" = blocks[["holdsOnly"]],
                     "allClearB" = blocks[["allClearB"]])
    blocks <- newBlocks
  }
  
  return(blocksOut)
}

# test <- blockGen(blockStruct="blockStructure.json", videosToUse="wordList.csv", stimDir="stimuli", maskColor="green", aws="http://meta.uchicago.edu", playBackrepetitions=5, transOnlyFirst=FALSE)
# 
# lapply(test, function(x){print.data.frame(x[["videos"]]); return(NULL)})
