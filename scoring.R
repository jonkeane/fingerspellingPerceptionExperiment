library(dplyr)
library(ggplot2)
source('dbUtils.R')

participantsessions <- loadData("participantsession")
wordResponses <- loadData("wordResp")

letters <- read.csv("ritaApogeesCleaned.csv")

words <- letters %>% group_by(word, wordBegin, wordEnd, wordList, repetition, wordInstanceID, wordtype, length, buttontype, signer, speed, segment, wordInstanceVideoPath, holdProp) %>% summarise()

letters

words <- read.csv("wordListASL3Students.csv")

words$videoName <- gsub("foo/", "", as.character(words$wordInstanceVideoPath))

answers <- as.character(words$word)
names(answers) <- words$videoName

wordResponses$correct <- {wordResponses$word == answers[gsub(".*stim([[:digit:]]+.mp4)", "\\1",  wordResponses$video)]}

print.data.frame(wordResponses %>% group_by(partsessionid, block) %>% summarise(rate = mean(correct)))
