library(jsonlite)
library(dplyr)
source("dbUtils.R")


blockGen <- function(blockStruct, videosToUse, stimDir, maskColor, aws="", playBackrepetitions=1, transOnlyFirst=FALSE, gAnalyticsID="") {
#   print(gAnalyticsID)
  
  # Concat the directory lsiting for the stimuli (including the aws path)
  stimDir <- paste(aws, stimDir, sep="/")

  # read in the stimuli words, and process them according to least seen.
  videosDF <- read.csv(videosToUse)
  
  #subset for english words only
  videosDF <- filter(videosDF, wordtype != "nonEnglish")
  
  # Connect to the database
  db <- fsdbConn()
  # Construct the fetching query
  query <- sprintf("SELECT wordResp.id as id, participantsession.gAnalyticsID, wordResp.timestamp, wordResp.video FROM wordResp, participantsession WHERE wordResp.partsessionid = participantsession.id;")
  # Submit the fetch query and disconnect
  prevData <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
  
#   wordRespData <- loadData("wordResp")
#   partSess <- loadData("participantsession")
#   prevData <- merge(wordRespData, partSess, by.x="partsessionid", by.y="id")
  
  prevData$video <- as.factor(prevData$video)
 
  # for separating based on condition. To use this, the video chunker would have to be re-engineered
  # prevRespCounts <- prevData %>% group_by(speed, maskcolor, masktype, video) %>% summarise(nResps = length(timestamp), nRespSameGAid = sum(gAnalyticsID.y == gAnalyticsID))
  prevRespCounts <- prevData %>% group_by(video) %>% summarise(nResps = length(timestamp), nRespSameGAid = sum(gAnalyticsID == gAnalyticsID))
  
  # remove directory info
  prevRespCounts$stimName <- gsub(".*(stim[[:digit:]]+.mp4)", "\\1",  prevRespCounts$video)
  
  # merge the whole videosDF with the previous response counts
  videosWithCounts <- merge(select(videosDF, stimName, word, signer), select(prevRespCounts, -video), all.x=TRUE)
  
  # change all NAs (stims that haven't been seen before) to 0 counts
  videosWithCounts$nResps[is.na(videosWithCounts$nResps)] <- 0
  videosWithCounts$nRespSameGAid[is.na(videosWithCounts$nRespSameGAid)] <- 0
  
  # summarize after grouping by stimname so that the counts for each condition are collapsed. sum or mean works here, mean should be less sensative to one stim getting a bunch in one block than sum.
  videosWithCounts <- videosWithCounts %>% group_by(stimName, word, signer) %>% summarise(nResps = mean(nResps, na.rm=TRUE), nRespSameGAid = mean(nRespSameGAid, na.rm=TRUE))
  
  # order, first the responseses with the same GA id, and then overall number of responses. (this will prefer stims this GA id has seen the least, and then after that prefer stims that have been seen the least.)
  videosWithCounts <- videosWithCounts[with(videosWithCounts, order(nRespSameGAid, nResps)), ]
  
  # determine weights from number of observations, weighting heavier for this GA having seen the stim, and then invert all of the probabilities so the least seen stimuli are the most likely.
  if(all(videosWithCounts$nResps == 0)){
    # If there is no data, use a flate probability
    videosWithCounts$prob <- 1/nrow(videosWithCounts)
  } else {
    # If there is any data at all, generate probabilites based on what has been seen already.
    addToFloor <- 0.0001
    numer = ((videosWithCounts$nRespSameGAid*5+videosWithCounts$nResps)/sum(videosWithCounts$nRespSameGAid*5+videosWithCounts$nResps)+addToFloor)
    denom = (sum(1/((videosWithCounts$nRespSameGAid*5+videosWithCounts$nResps)/sum(videosWithCounts$nRespSameGAid*5+videosWithCounts$nResps)+addToFloor)))
    videosWithCounts$prob<-1/numer/denom
  }

#   # convert to character vector for simple ordering
#   videos <- as.character(videosWithCounts$stimName)

  # sample without replacement for weighted probability ordering
  videosWithCountsOrdered <- videosWithCounts[sample(1:nrow(videosWithCounts), nrow(videosWithCounts), replace=FALSE, prob = videosWithCounts$prob),]
  
  # deduplicate words
  videosWithCountsOrderedDedupped <- videosWithCountsOrdered[!duplicated(videosWithCountsOrdered$word),]
  # tests
  # print.data.frame(videosWithCountsOrderedDedupped)
  # print(xtabs(~word, videosWithCountsOrderedDedupped))

  
  # grab structure and messages from the external json file.
  blocks <- fromJSON(blockStruct)
  chunks <- sapply(blocks, function(list){return(list[["numStims"]])})

  # ensure that each signer is equaly represented
  nSigners <- length(unique(videosWithCountsOrderedDedupped$signer))

  videos <- videosWithCountsOrderedDedupped %>% group_by(signer) %>% do(head(., sum(chunks)/nSigners))
  # test
  # print.data.frame(videos)
  # print(xtabs(~signer, videos))
  
  # sample again to randomize the resulting order.
  videos <- sample(as.character(videos$stimName), replace = FALSE)
  
  # cut the list of videos to use into chunks based on the block descriptions
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

# nightwatchKeyGen(videosToUse="wordList.csv")

# testBlock <- blockGen(blockStruct="blockStructure.json", videosToUse="wordList.csv", stimDir="stimuli", maskColor="green", aws="http://meta.uchicago.edu", playBackrepetitions=5, transOnlyFirst=FALSE)

# 
# testRandom <- function(n){
#   block <- blockGen(blockStruct="blockStructure.json", videosToUse="wordList.csv", stimDir="stimuli", maskColor="green", aws="http://meta.uchicago.edu", playBackrepetitions=5, transOnlyFirst=FALSE)
#   dflist <- lapply(block, function(blk){blk[["videos"]]})
#   dfOut <- do.call("rbind", dflist)
#   dfOut$n <- n
#   return(dfOut)
# }
# 
# randoSample <- lapply(1:100, testRandom)
# 
# outRandom <- do.call(rbind, randoSample)
# 
# # merge with wordlist, to checkout various grouping effects.
# videosDF <- read.csv("wordList.csv")
# videosDF$stim <- as.character(videosDF$stimName)
# outRandom$stim <- as.character(outRandom$stim)
# outRandom <- left_join(select(outRandom, -video), select(videosDF, stim, word, signer), by="stim")
# 
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
# # view distribution of stimuli, including the practice rows (so the all clear distribution is shifted higher.
# subset(outRandom, !grepl("practice", rownames(outRandom))) %>% group_by(signer, maskType) %>% do(data.frame(rows = nrow(.))) %>% ggplot(.) + aes(x=rows) + geom_histogram() + facet_wrap(signer~maskType, ncol=1)


# 
# lapply(test, function(x){print.data.frame(x[["videos"]]); return(NULL)})





