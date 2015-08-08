blockGen <- function(videosToUse, stimDir, maskColor, aws="", playBackrepetitions=1, transOnlyFirst=FALSE) {
  
stimDir <- paste(aws, stimDir, sep="/")

blocks <- 
  list("practice" =
         list(
           "message" =
             "<h4>First, try a few practice items. <br/> <br/>  Type the word that was fingerspelled. If you don’t know the exact word, type the letters you think you saw.</h4>",
           "videos" =
             data.frame(
               "num" = c(1:4),
               "video" = file.path(stimDir, maskColor, "allClear", videosToUse[1:4]),
               rep = playBackrepetitions,
               stringsAsFactors = FALSE
             )
         ),
       "allClearA" =
         list(
           "message" =
             "<h4>Good job! Now we will begin the expeirment. <br/> <br/> Type the word that was fingerspelled. If you don’t know the exact word, type the letters you think you saw.</h4>",
           "videos" =
             data.frame(
               "num" = c(1:15),
               "video" = file.path(stimDir, maskColor, "allClear", videosToUse[5:19]),
               rep = playBackrepetitions,
               stringsAsFactors = FALSE
             )
         ),
       "holdsOnly" =
         list(
           "message" =
             "<h4>Take a little break if you like. <br/> <br/> Now you’ll see fingerspelled videos very similar to the ones you’ve seen already, but this time a black screen is inserted to block certain parts of the video. Just as before, do your best to understand the word that was fingerspelled. <br/> <br/>  Type the word you saw. If you aren’t sure, type the letters you saw. <br/> <br/> for testing: <b>holds only</b></h4>",
           "videos" =
             data.frame(
               "num" = c(1:30),
               "video" = file.path(stimDir, maskColor, "holdsOnly", videosToUse[20:49]),
               rep = playBackrepetitions,
               stringsAsFactors = FALSE
             )
         ),
       "transitionsOnly" =
         list(
           "message" =
             "<h4>Take a little break if you like. <br/> <br/> Now you’ll see fingerspelled videos very similar to the ones you’ve seen already, but this time a black screen is inserted to block certain parts of the video. Just as before, do your best to understand the word that was fingerspelled. <br/> <br/>  Type the word you saw. If you aren’t sure, type the letters you saw.  <br/> <br/> for testing: <b>transitions only</b></h4>",
           "videos" =
             data.frame(
               "num" = c(1:30),
               "video" = file.path(stimDir, maskColor, "transOnly", videosToUse[50:79]),
               rep = playBackrepetitions,
               stringsAsFactors = FALSE
             )
         ),
       "allClearB" =
         list(
           "message" =
             "<h4>Almost done! Take a little break if you like. <br/> <br/> Now you’ll see fingerspelled videos very similar to the first ones you saw. Just as before, do your best to understand the word that was fingerspelled. <br/> <br/>  Type the word you saw. If you aren’t sure, type the letters you saw.</h4>",
           "videos" =
             data.frame(
               "num" = c(1:15),
               "video" = file.path(stimDir, maskColor, "allClear", videosToUse[80:94]),
               rep = playBackrepetitions,
               stringsAsFactors = FALSE
             )
         )
       )

# there ought to be an easier way to do this.
if(transOnlyFirst){
  newBlocks = list("practice" = blocks[["practice"]],
                   "allClearA" = blocks[["allClearA"]],
                   "transitionsOnly" = blocks[["transitionsOnly"]],
                   "holdsOnly" = blocks[["holdsOnly"]],
                   "allClearB" = blocks[["allClearB"]])
  blocks <- newBlocks
}

return(blocks)
}

# test <- blockGen(videosToUse, 2, transOnlyFirst = TRUE)
# test <- blockGen(videosToUse, 2, transOnlyFirst = FALSE)


