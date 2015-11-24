library(plyr)
library(dplyr)
library(lme4)
library(arm)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
source("dbUtils.R")
source("ggCaterpillar.R")
source("CoefficientPlot.R")
source("predictions.R")

# grab participant data
participantsessions <- loadData("participantsession")
participantsessions$startTime <- ymd_hms(participantsessions$startTime)
participantsessions$follPartStartTime <- NA
participantsessions[1:nrow(participantsessions)-1,]$follPartStartTime <- participantsessions[2:nrow(participantsessions),]$startTime
participantsessions$follPartStartTime <- as.POSIXct(participantsessions$follPartStartTime, origin = "1970-01-01",tz = "GMT")

# grab response data
wordResponses <- loadData("wordResp")

# read in the words.
words <- read.csv("wordListASL3Students.csv")

# read in mapping from study codes to training types, gender, comments.
studyCodesToTests <- read.csv("codesToTest.csv")
studyCodesToTests$studyCode <- as.character(studyCodesToTests$studyCode)

# subset the video name to be just stim[#].mp4
words$videoName <- gsub("foo/", "", as.character(words$wordInstanceVideoPath))

# generate a named list that contains the answers
answers <- as.character(words$word)
names(answers) <- words$videoName


wordResponses$word <- gsub(" ", "", wordResponses$word)
wordResponses$timestamp <- as.numeric(wordResponses$timestamp)
wordResponses$timestampUTC <- as.POSIXct(wordResponses$timestamp, origin = "1970-01-01",tz = "GMT")

wordResponses$stimWord <- answers[gsub(".*stim([[:digit:]]+.mp4)", "\\1",  wordResponses$video)]
wordResponses$correct <- {wordResponses$word == wordResponses$stimWord}

wordResponses$orientation <- as.factor(ifelse(grepl("[ghpq]+",as.character(wordResponses$stimWord)),"Non-default","Default"))

# uniqueWordResponses <- unique(wordResponses)

# filter and uppercase studycodes
realParticipantsessions <- filter(participantsessions, !{studyCode %in% c("nightwatchtest")})
realParticipantsessions$studyCode <- toupper(realParticipantsessions$studyCode)

# Merge with responses and participant data
fullData <- merge(wordResponses, realParticipantsessions, by.x="partsessionid", by.y="id")

# Merge with study codes identifying the training types.
fullData <- merge(fullData, studyCodesToTests)


# Check that the videos they observed match the blocks:
fullData$blockVid <- gsub("https://s3.amazonaws.com/fingerspelling-perception/stimuli/half/black/", "", as.character(fullData$video))
fullData <- separate(fullData, blockVid, c("vidCond", "vidDispl"), sep = "/")

fullData$vidBlockMatch <- fullData$vidCond == ifelse(fullData$block %in% c("allClearA","allClearB", "practice"), "allClear", ifelse(fullData$block == "transitionsOnly", "transOnly", fullData$block))
# all but 74, below matches.

##### regroup, based on time and  ###################################
testSet <- filter(fullData)

# patsessionid == 74, studycode == D2T47, the last allclear, is actually called holdsOnly, but the stimuli video is really from allclear, so change it.
# testSet[testSet$timestamp == 1442425519, ]$video
# testSet[testSet$timestamp == 1442425519, ]$block <- "allClearA"

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

View(select(filter(data, expComplete == FALSE), partsessionid , id , word , timestamp  , numInBlock , block , repetitions , playCount , timestampUTC , maxNumInBlock , realNumInBlock , blockComplete , blockAllSeen , blockInfo , numBlocksSeen , expComplete , expAllSeen))


### Analysis of participant interactions
studyCodes <- unique(select(data, partsessionid, studyCode, expComplete, expAllSeen))

print.data.frame(studyCodes)

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



# fit <- glmer(correct~block*orientation+(1|numInBlock)+(1+block*orientation|stimWord)+(1+block*orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))

fit.0 <- glmer(correct~block*orientation*training+(1|numInBlock)+(1+block+orientation+training|stimWord)+(1+block+orientation+training|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
summary(fit.0)
CoefficientPlot(list(fit.0))

ggplot(pred(fit.0)) + aes(x=block, y=invlogit(correct), ymin=invlogit(plo), ymax=invlogit(phi)) + geom_pointrange()  + facet_grid(orientation~training)+ ylim(0,1) + labs(title = "Model predictions for accuracy", x = "block", y = "probability of correct response") 


predFun <- function(fitModel) {
  vars <- str_split(as.character(fitModel@call$formula), fixed(" "))
  outcomeVarName <- vars[[2]]
  
  newDat <- fitModel@frame
  newDat[,outcomeVarName] <- NA
  newDat <- unique(newDat)
  
  predict(fitModel,newDat)
}

predLabelFun <- function(fitModel) {
  vars <- str_split(as.character(fitModel@call$formula), fixed(" "))
  outcomeVarName <- vars[[2]]
  
  newDat <- fitModel@frame
  newDat[,outcomeVarName] <- NA
  newDat <- unique(newDat)
  
  newDat
}

system.time(
bbnew <- bootMer(fit.0, nsim=1000, FUN=predFun, seed=101, parallel="multicore", ncpus = parallel::detectCores(), use.u = TRUE)
)

test <- left_join(add_rownames(predLabelFun(fit.0), var = "rowname"), gather(as.data.frame(bb$t), rowname, pred)) %>% group_by(cat, subj) %>% summarise(plo = quantile(pred, probs = c(0.025), na.rm=TRUE), contact = quantile(pred, probs = c(0.5), na.rm=TRUE), phi = quantile(pred, probs = c(0.975), na.rm=TRUE))


# fit.1 <- glmer(correct~block*orientation+(1|numInBlock)+(1+block|stimWord)+(1+block+orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
# summary(fit.1)

# fit.2 <- glmer(correct~block*orientation+(1|numInBlock)+(1|stimWord)+(1+block+orientation|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
# summary(fit.2)


fit.0 <- glmer(correct~block*training+(1|numInBlock)+(1+block+training|stimWord)+(1+block+training|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
summary(fit.0)

fit.1 <- glmer(correct~orientation*training+(1|numInBlock)+(1+training|stimWord)+(1+training|partsessionid), cleanData, family=binomial(link="logit"),na.action="na.omit", control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=4000000)))
summary(fit.1)

