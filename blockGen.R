library(jsonlite)
library(dplyr)


blockGen <- function(blockStruct, videosToUse, stimDir, maskColor, aws="", playBackrepetitions=1, transOnlyFirst=FALSE, gAnalyticsID="") {
#   print(gAnalyticsID)
  
  # Concat the directory lsiting for the stimuli (including the aws path)
  stimDir <- paste(aws, stimDir, sep="/")

  # read in the stimuli words, and process them according to least seen.
  videosDF <- read.csv(videosToUse)
  
  #subset for english words only
  videosDF <- filter(videosDF, wordtype != "nonEnglish")

  # sample without replacement for weighted probability ordering sample again to randomize the resulting order.
  videos <- as.character(sample(videosDF$stimName, nrow(videosDF), replace=FALSE))

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

##### write json file for nightwatch tests ###################################
nightwatchKeyGen <- function(videosToUse, path="./stimAns.json"){
  # read in the stimuli words, and process them according to least seen.
  videosDF <- read.csv(videosToUse)
  
  out <- as.list(as.character(videosDF$word))
  out <- setNames(out, as.character(videosDF$stimName))

  write(toJSON(out), file = path)
}


##### write json file for nightwatch captchasl tests ###################################
# robotList <- read.csv("robot.csv", stringsAsFactors = FALSE)
# robotKey <- as.list(strsplit(robotList$answer, ","))
# robotKey <- setNames(robotKey, robotList$video)
# 
# write(toJSON(robotKey), file = "./captchASLans.json")


##### tests ###################################

# nightwatchKeyGen(videosToUse="wordListASL3Students.csv")
# 
# testBlock <- blockGen(blockStruct="blockStructure.json", videosToUse="wordListASL3Students.csv", stimDir="stimuli", maskColor="green", aws="http://localhost", playBackrepetitions=5, transOnlyFirst=FALSE)
# 
# testRandom <- function(n){
#   block <- blockGen(blockStruct="blockStructure.json", videosToUse="wordListASL3Students.csv", stimDir="stimuli", maskColor="green", aws="http://localhost", playBackrepetitions=5, transOnlyFirst=FALSE)
#   dflist <- lapply(block, function(blk){blk[["videos"]]})
#   dfOut <- do.call("rbind", dflist)
#   dfOut$n <- n
#   return(dfOut)
# }
# 
# randoSample <- lapply(1:1000, testRandom)
# 
# outRandom <- do.call(rbind, randoSample)
# 
# 
# unique(outRandom) %>% group_by(n) %>% do(data.frame(uniStimsN = length(unique(.$stim)), stimsN = length(.$stim))) -> testRandomResults
# 
# View(filter(outRandom, n==1))
# 
# # view distribution of stimuli, including the practice rows (so the all clear distribution is shifted higher.
# outRandom %>% group_by(stim, maskType) %>% do(data.frame(rows = nrow(.))) %>% ggplot(.) + aes(x=rows) + geom_histogram() + facet_wrap(~maskType, ncol=1)
# 
# # view distribution of stimuli, excluding the practice rows.
# subset(outRandom, !grepl("practice", rownames(outRandom))) %>% group_by(stim, maskType) %>% do(data.frame(rows = nrow(.))) %>% ggplot(.) + aes(x=rows) + geom_histogram() + facet_wrap(~maskType, ncol=1)

# 
# lapply(test, function(x){print.data.frame(x[["videos"]]); return(NULL)})





