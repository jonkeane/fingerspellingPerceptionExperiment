labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

BGquesGen <- function(question, aws, video=TRUE, text=TRUE) {
  divOut <- list(
    if(video & question$video != ""){
      tags$video(src=paste(aws, "skilledASLbg", question$video, sep="/"), type = "video/mp4", controls=TRUE, width = 640 )
    },
    tags$br(),
    if(text & question$translation != ""){
      if(!is.na(question$required) & question$required){
        labelMandatory(question$translation)
      } else {
        question$translation
      }
    },
    if(question$inputType=="text"){
      textInput(question$qName, NULL, NULL)
    } else if(question$inputType=="integer"){
      numericInput(question$qName, NULL, NULL, min=0, max=120)
    } else if(question$inputType=="select"){
      selectInput(question$qName, NULL, selected = NULL, choices = unlist(strsplit(question$options, split=",")))
    } else if(question$inputType=="yesno"){
      selectInput(question$qName, NULL, selected = NULL, choices = c("yes","no"))
    }
  )
  return(divOut)
}


# langBG <- list()
# for(i in 1:nrow(languageBG)){
#   langBG <- append(langBG,BGquesGen(languageBG[i,], video=FALSE))
# }