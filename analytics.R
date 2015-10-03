library(dplyr)
library(ggplot2)
source('dbUtils.R')

participantsessions <- loadData("participantsession")
wordResponses <- loadData("wordResp")

realResp <- filter(wordResponses, partsessionid > 3)
realResp$timestamp <- as.numeric(realResp$timestamp)

print.data.frame(realResp %>% group_by(partsessionid) %>% summarise(duration = (max(timestamp)-min(timestamp))/60))

wordResponses$stim <- as.factor(gsub(".*(stim[[:digit:]]+.mp4)", "\\1",  wordResponses$video))
wordResponses$partsessionid <- as.factor(wordResponses$partsessionid)
names(wordResponses)


View(filter(wordResponses, partsessionid==24))


ggplot(wordResponses) + aes(x=stim, y=..count..) + geom_point(stat="bin")

wordResponses %>% group_by(stim, masktype) %>% do(data.frame(rows = nrow(.))) -> test

ggplot(test) + aes(x=rows) + geom_histogram() + facet_wrap(~masktype, ncol=1)

7282/519

length(unique(wordResponses$stim))

unique(select(wordResponses, -word)) -> dedupWordResponses

unique(select(wordResponses, -word)) %>% group_by(partsessionid) %>% do(data.frame(uniStimsN = length(unique(.$stim)), stimsN = length(.$stim))) -> test

View(filter(dedupWordResponses, partsessionid==19))
