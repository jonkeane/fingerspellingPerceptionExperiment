library(shiny)
library(digest)
source("dbUtils.R")
source("BGquesGen.R")
source("blockGen.R")


##### Set variables, and load video lists.
# robot prevention / captcha
robotList <- read.csv("robot.csv", stringsAsFactors = FALSE)
robotKey <- robotList$answer
names(robotKey) <- robotList$video

# language background
languageBG <- read.csv("studentBGQues.csv", stringsAsFactors = FALSE)
# take options and turn them into list.

# mandatory and fields for the experiment
fieldsMandatory <- c("word")
fieldsAll <- c("word")

# mandatory and fields for the language background
fieldsMandatoryBG <- subset(languageBG, required==TRUE)$qName
fieldsAllBG <- languageBG$qName

#stimuli video setup
stimDir <- "stimuli"
maskColor <- "green"
aws <- "http://localhost"
# aws <- "https://s3.amazonaws.com/fingerspelling-perception"

# # for writing the stim csv
# webDir <- "www"
# condition <- "allClear"
# searchPath <- file.path(webDir, stimDir, maskColor, condition)
# appendPath <- file.path(stimDir, maskColor, condition)
#
# videos <- file.path(appendPath, list.files(path=searchPath))
# videos <- list.files(path=searchPath)
# write.csv(videos, "videoList.csv", row.names = FALSE, col.names = FALSE)

videoCSV <- "wordList.csv"


##### functions for convenience
sqlStrip <- function(string) {
  # remove any character that's not alphanumeric, a dot, a space, or a hyphen
  string <- gsub("[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890\\. \\-]", "", string)
  return(string)
}

randomRows <- function(df,n, nonRandom=FALSE, ...){
  if(nonRandom){
    # for when this should not be random
    return(df[1:n,])
  } else {
    #sample rows from a df from http://stackoverflow.com/questions/8273313/random-rows-in-dataframe-in-r
    return(df[sample(nrow(df),n, ...),])
  }
}

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

responsesDir <- file.path("responses")

epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")




######### Shiny server
shinyServer(function(input, output, session) {
  ##### functions ###########################################################
  wordData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    # strip anything that's not alphanumeric off of the input. This could be replaced with escapes.
    data <- sqlStrip(data)
    data <- c(partsessionid = sessValues$participantID, data, timestamp = epochTime(), video = sessValues$videoUp$video, numInBlock = sessValues$videoUp$num, block = names(sessValues$blocks[1]), repetitions = sessValues$videoUp$rep, speed = sessValues$videoUp$speed, maskcolor = sessValues$videoUp$maskColor, masktype = sessValues$videoUp$maskType, playCount = input$playCount)
    data
  })

  bgData <- reactive({
    data <- sapply(fieldsAllBG, function(x) input[[x]])
    # strip anything that's not alphanumeric off of the input. This could be replaced with escapes.
    data <- sqlStrip(data)
    data <- c(id = NULL, data, gAnalyticsID = sessValues$gAnalyticsID, startTime = humanTime()) # The null might not need to be here, alternately, this could be listified.
    data
  })

  saveData <- function(data, table) {
    # Connect to the database
    db <- fsdbConn()
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table,
      paste(c(names(data)), collapse = ", "),
      paste(c(data), collapse = "', '")
    )

    # Submit the update query and disconnect
    dbGetQuery(db, query)
    lastInsert <- dbGetQuery(db, "SELECT LAST_INSERT_ID();")[1,1]
    dbDisconnect(db)

    return(lastInsert)
  }

  saveWordResp <- function(){
    if(!is.null(input[["word"]])){
    # detect if there is data to save first!
      saveData(wordData(), table="wordResp")
      reset("form")
    }
  }


  nextBlock <- function(){
    output$page <- renderUI({
      # Scroll to the top
      session$sendCustomMessage(type = 'scrollToTop', message=list())

      # the extended javascript requires arguments to be submitted individually, and unlisting with do.call and the like does not seem to work. Very hacky.
      # do.call(js$updateVideoCache, videoCache)
      session$sendCustomMessage(type = 'updateVideoCache', message = list(
        sessValues$blocks[[1]]$videos[1,]$video,
        sessValues$blocks[[1]]$videos[2,]$video,
        sessValues$blocks[[1]]$videos[3,]$video,
        sessValues$blocks[[1]]$videos[4,]$video,
        sessValues$blocks[[1]]$videos[5,]$video,
        sessValues$blocks[[1]]$videos[6,]$video,
        sessValues$blocks[[1]]$videos[7,]$video,
        sessValues$blocks[[1]]$videos[8,]$video,
        sessValues$blocks[[1]]$videos[9,]$video,
        sessValues$blocks[[1]]$videos[10,]$video
        ))

      # display splash screen between blocks
      div(
        id = "pause",
        HTML(sessValues$blocks[[1]]$message),
        uiOutput("continueButton"),
        align = "center",
        style = "padding: 50px;"
      )
    })

    output$continueButton <- renderUI({
#       actionButton("continue", label = "please wait...", class = "btn-primary", disabled = TRUE)
        actionButton("continue", label = "continue", class = "btn-primary")
    })
  }


  advance <- function(){
    if(nrow(sessValues$blocks[[1]]$videos)==0){
      # change to the next block
      sessValues$blocks <- tail(sessValues$blocks, length(sessValues$blocks)-1)
      if(length(sessValues$blocks)==0){
        output$page <- renderUI({
          # Scroll to the top
          session$sendCustomMessage(type = 'scrollToTop', message=list())

          div(
            id = "thankyou_msg",
            h3("Thanks, your response was submitted successfully!")
          )
      })
      } else {
        nextBlock()
      }
    } else {
      output$page <- renderUI({list(
        # Scroll to the top
        session$sendCustomMessage(type = 'scrollToTop', message=list()),

        #display the video, disable the context menu to attempt to stop people from playing the video more than once.
        div(
          id = "stimuliVideo",
          oncontextmenu="return false;",
          align = "center",
          style = "padding: 15px;"
        ),
        div(
          id = "form",
          textInput("word", labelMandatory("What word was fingerspelled?"), ""),
          actionButton("submit", "submit", class = "btn-primary"),
          align = "center"
        )
      )})

      # grab the next video and then store the videos list less the first
      sessValues$videoUp <- head(sessValues$blocks[[1]]$videos, 1)
      sessValues$blocks[[1]]$videos <- tail(sessValues$blocks[[1]]$videos, nrow(sessValues$blocks[[1]]$videos)-1)

      if(!is.na(sessValues$blocks[[1]]$videos[10,]$video)){
        session$sendCustomMessage(type = 'updateVideoCache', message = list(sessValues$blocks[[1]]$videos[10,]$video))
      }

      # the message needs to be formed *outside* of the onFlushed function.
      # possibly related to? https://github.com/daattali/shinyjs/issues/39
      msg <- list(video = sessValues$videoUp$video, rep = sessValues$videoUp$rep)
      session$onFlushed(function(){
        session$sendCustomMessage(type = 'changeVideo', message = msg)
        })

    }
  }

  ##### Start the experiment ######################################################################
  # setup reactive values
  sessValues <- reactiveValues()
  sessValues$gAnalyticsID <- NULL
  sessValues$blocks <- NULL
  sessValues$videoUp <- ""

  observe({
    sessValues$gAnalyticsID <- input$gaClientID
  })

  observeEvent(input$grabStims, {
    print("grabbing, false")
    if(input$grabStims==1){
      print("grabbing, true")
      # grab stim list
      progress <- shiny::Progress$new()
      progress$set(message = "Generating videos", value = 0)

      n <- 10
      for (i in 1:n) {
        tryCatch({
          print(sessValues$blocks)
          Sys.sleep(5)
          sessValues$blocks <- blockGen(blockStruct="blockStructure.json", videosToUse="wordList.csv", stimDir="stimuli", maskColor="green", aws=aws, playBackrepetitions=2, gAnalyticsID=sessValues$gAnalyticsID)
          print(sessValues$blocks)
        },
        error = function(e) {
          # Increment the progress bar, and update the detail text.
          progress$set(1/n, detail = paste("Trying again: ", i))
        })
        if(!is.null(sessValues$blocks)){
          progress$set(1, detail = paste("Loaded!"))
          progress$close()
          break
        }
      }
    }
  })


  # generate a subset of robot checking answers, and display them.
  # if 0 rows are specified, then skip this whole thing?
  robotSubList <- randomRows(robotList, 4, nonRandom=FALSE)
  robotElemList <- list(tags$video(src=paste(aws, "robot", "robotPrevention.mp4", sep="/"), type = "video/mp4", controls=TRUE, width = 640, autoplay=TRUE),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br())

  for(n in 1:nrow(robotSubList)){
    vid <- robotSubList[n,]$video
    robotElemList <- append(robotElemList, list(tags$video(src=paste(aws, "robot", vid, sep="/"), type = "video/mp4", controls=TRUE, width = 640 ),
    textInput(paste(vid), NULL),
    tags$br(),
    tags$br()))
  }

  output$page <- renderUI({
    # Scroll to the top
    session$sendCustomMessage(type = 'scrollToTop', message=list())

    # Captcha section
    div(
      id = "robotForm",
      h2("Fingerspelling study – introduction", style = "padding: 15px;"),
      robotElemList,
      actionButton("robotSubmit", label = "submit", class = "btn-primary", disabled = TRUE),
      align = "center"
    )
  })


  observeEvent(input$robotSubmit, {
    data <- sapply(robotSubList$video, function(x) input[[x]])
    # strip anything that's not alphanumeric off of the input. This could be replaced with escapes.
    data <- sqlStrip(data)
    data <- gsub(" ", "", data)
    data <- tolower(data)
    ans <- data %in% unlist(strsplit(robotKey[names(data)], ","))

    # dump answers to database for analysis later
    captchaData <- list(gAnalyticsID = sessValues$gAnalyticsID, video = names(data), response = unname(data), correct = as.integer(ans))
    saveData(captchaData, table="captchASL")

    if(all(ans)){
      # they got all of the robotchecking captcha questions correct, proceed to language background
      langBG <- list()
      for(i in 1:nrow(languageBG)){
        langBG <- append(langBG,BGquesGen(languageBG[i,], aws = aws, video=FALSE, text=TRUE))
      }

      output$page <- renderUI({
        # language background
        div(
          id = "languageBG",
          h2("Language background", style = "padding: 15px;"),
          langBG,
          actionButton("languageBGSubmit", "submit", class = "btn-primary"),
          align = "center"
        )
      })

      session$onFlushed(function() {
        session$sendCustomMessage(type="grabStimuliList", list(TRUE))
        # sessValues$grabStims <- TRUE
      })


    } else {
      output$page <- renderUI({
        # Scroll to the top
        session$sendCustomMessage(type = 'scrollToTop', message=list())

        div(
          id = "thankyou_msg",
          h3("Sorry, you do not qualify for the study.")
        )
      })
    }

    # Scroll to the top
    session$sendCustomMessage(type = 'scrollToTop', message=list())
    })

  observeEvent(input$languageBGSubmit, {
    sessValues$participantID <-  saveData(bgData(), table="participantsession")
    # start the experiment
    nextBlock()
  })


  # Check if the word is filled out.
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    # enable/disable the submit button
    toggleState(id = "submit", condition = mandatoryFilled)
  })

  observe({
    # check if all Robot fields have a value
    mandatoryFilled <-
      vapply(robotSubList[1:nrow(robotSubList),]$video,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    # enable/disable the submit button
    toggleState(id = "robotSubmit", condition = mandatoryFilled)
  })

  # Check if all of the required language background items were filled out
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatoryBG,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    # enable/disable the submit button
    toggleState(id = "languageBGSubmit", condition = mandatoryFilled)
  })

  # check if precaching is done on the pause screens
#   observe({
#     if(!is.null(input$donePrecaching)){
#       if(input$donePrecaching==1){
#         # enable/disable the continue button # doesn't work currently
# #         enable(id = "continue")
#         # If either of these messages / UI changes are enabled, the pauseMessage stops updating
#         output$continueButton <- renderUI({
#           actionButton("continue", label = "continue", class = "btn-primary")
#         })
#       }
#     } else {
# #       disable(id = "continue")
#     }
#   })


  # action to take when the continue button is pressed
  observeEvent(input$continue, {
    advance()
    })


  # action to take when submit button is pressed
  observeEvent(input$submit, {
    saveWordResp()
    advance()
  })

  # action to take when the enter key is pressed
  observeEvent(input$keysPressed, {
    if(input$keysPressed == 13 & {all(vapply(fieldsMandatory,
                    function(x) {
                      !is.null(input[[x]]) && input[[x]] != ""
                    },
                    logical(1)))}){
      saveWordResp()
      advance()
    }
})

})
