library(dplyr)
library(lme4)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)

participantsessions <- loadData("participantsession")
participantsessions$startTime <- ymd_hms(participantsessions$startTime)
participantsessions$follPartStartTime <- NA
participantsessions[1:nrow(participantsessions)-1,]$follPartStartTime <- participantsessions[2:nrow(participantsessions),]$startTime
participantsessions$follPartStartTime <- as.POSIXct(participantsessions$follPartStartTime, origin = "1970-01-01",tz = "GMT")


wordResponses <- loadData("wordResp")

words <- read.csv("wordList.csv")

words$videoName <- gsub("wordInstanceVideos/", "", as.character(words$wordInstanceVideoPath))

answers <- as.character(words$word)
names(answers) <- words$videoName


wordResponses$word <- gsub(" ", "", wordResponses$word)
wordResponses$timestamp <- as.numeric(wordResponses$timestamp)
wordResponses$timestampUTC <- as.POSIXct(wordResponses$timestamp, origin = "1970-01-01",tz = "GMT")

wordResponses$stimWord <- answers[gsub(".*stim([[:digit:]]+.mp4)", "\\1",  wordResponses$video)]
wordResponses$correct <- {wordResponses$word == wordResponses$stimWord}

wordResponses$orientation <- as.factor(ifelse(grepl("[ghpq]+",as.character(wordResponses$stimWord)),"Non-default","Default"))

uniqueWordResponses <- unique(wordResponses)

realParticipantsessions <- filter(participantsessions, !{studyCode %in% c("crashtestdummy", "jontestmacprochrome", "jontestsafarimacpro")})
realParticipantsessions$studyCode <- toupper(realParticipantsessions$studyCode)

fullData <- merge(select(uniqueWordResponses, -gAnalyticsID), realParticipantsessions, by.x="partsessionid", by.y="id")

# Check that the videos they observed match the blocks:
fullData$blockVid <- gsub("http://localhost/stimuli/regular/green/", "", as.character(fullData$video))
fullData <- separate(fullData, blockVid, c("vidCond", "vidDispl"), sep = "/")

fullData$vidBlockMatch <- fullData$vidCond == ifelse(fullData$block %in% c("allClearA","allClearB", "practice"), "allClear", ifelse(fullData$block == "transitionsOnly", "transOnly", fullData$block))
# all but 74, below matches.

##### regroup, based on time and  ###################################
testSet <- filter(fullData)

# patsessionid == 74, studycode == D2T47, the last allclear, is actually called holdsOnly, but the stimuli video is really from allclear, so change it.
testSet[testSet$timestamp == 1442425519, ]$video
testSet[testSet$timestamp == 1442425519, ]$block <- "allClearA"

testSet$blockFact <- factor(testSet$block, levels = c("practice", "allClearA", "holdsOnly", "transitionsOnly", "allClearB"))



# check to see completion
blockNums <- list("practice"=4,
                  "allClearA"=15,
                  "holdsOnly"=30,
                  "transitionsOnly"=30,
                  "allClearB"=15 )


data <- fullData %>% 
  group_by(block, partsessionid) %>% 
  mutate(maxNumInBlock=max(numInBlock),
         realNumInBlock=length(numInBlock),
         blockComplete={max(numInBlock)==blockNums[block]},
         blockAllSeen={max(numInBlock)==blockNums[block] & length(numInBlock) == blockNums[block]}, 
         blockInfo=if(max(numInBlock)==blockNums[block] & length(numInBlock) == blockNums[block]){"complete"}
         else if(length(numInBlock) > blockNums[block]){"more seen"}
         else if(length(numInBlock) < blockNums[block]){"less seen"}
  )

data <- data  %>% group_by(partsessionid) %>% mutate(numBlocksSeen=length(unique(block)))
data <- data  %>% group_by(partsessionid) %>% mutate(expComplete=all(blockComplete), expAllSeen=all(blockAllSeen))

# View(filter(data, blockInfo == "more seen"))

### Analysis of participant interactions
studyCodes <- unique(select(data, partsessionid, studyCode, expComplete, expAllSeen))

# View(studyCodes[duplicated(studyCodes$studyCode)|duplicated(studyCodes$studyCode, fromLast=TRUE),])

# studyCodes that started, but did not finish the experiment, and that did not try again, subsquently finishing.
unique(filter(data, !expComplete)$studyCode)[!{unique(filter(data, !expComplete)$studyCode) %in% unique(filter(data, expComplete)$studyCode)}]

# studyCodes that started and finished and saw exactly five blocks.
unique(filter(data, numBlocksSeen<5)$studyCode)


# studyCodes that started and finished and saw exactly five blocks.
unique(filter(data, expComplete & expAllSeen & numBlocksSeen==5)$studyCode)

cleanData <- filter(data, expComplete & expAllSeen & numBlocksSeen==5 & block != "practice")

length(unique(cleanData$partsessionid))*(15+30+30+15) == nrow(cleanData)

cleanData$block <- factor(cleanData$block, levels = c("allClearA", "holdsOnly", "transitionsOnly", "allClearB"))



fit <- glmer(correct~block*orientation+(1|numInBlock)+(1+block*orientation|stimWord)+(1+block*orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))

fit.0 <- glmer(correct~block*orientation+(1|numInBlock)+(1+block+orientation|stimWord)+(1+block+orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
summary(fit.0)

fit.1 <- glmer(correct~block*orientation+(1|numInBlock)+(1+block|stimWord)+(1+block+orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
summary(fit.1)

fit.2 <- glmer(correct~block*orientation+(1|numInBlock)+(1|stimWord)+(1+block+orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
summary(fit.2)
