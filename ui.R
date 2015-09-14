library(shiny)
library(shinyjs)

appCSS <- ".mandatory_star { color: red; }"

shinyUI(fluidPage(
  tags$head(
    includeScript("googleAnalytics.js"), 
    includeScript("changeVideo.js"),
    includeScript("updateVideoCache.js"),
    tags$script("Shiny.addCustomMessageHandler('scrollToTop', function(params){window.scrollTo(0,0);})")
    ),
  useShinyjs(),
  inlineCSS(appCSS),
  tags$script(src="js/html5Preloader.js"),
  tags$script(HTML('myLoader = new html5Preloader();')),
  tags$script(HTML('ga(function(tracker) {
                    var clientId = tracker.get("clientId");
                    Shiny.onInputChange("gaClientID", clientId);
                   });')),
# Disabled because it fails to work randomly in safari.
#   tags$script(HTML('
#                   // myLoader.on("finish", function(){  console.log("All assets loaded."); });
#                   myLoader.on("finish", function(){
#                     Shiny.onInputChange("donePrecaching", 1); 
#                   });
#   ')),

  tags$script(HTML("
                  // from http://stackoverflow.com/questions/19492776/how-to-reset-scroll-position-in-a-div-using-javascript
                  function resetScrollPos() {
                    var divs = document.querySelectorAll('.mblScrollableViewContainer');
                    for (var p = 0; p < divs.length; p++) {
                      if (Boolean(divs[p].style.transform)) { //for IE(10) and firefox
                        divs[p].style.transform = 'translate3d(0px, 0px, 0px)';
                      } else { //for chrome and safari
                        divs[p].style['-webkit-transform'] = 'translate3d(0px, 0px, 0px)';
                      }
                    }
                  }
  ")),

  div(
    id = "page",
    uiOutput("page"),
    align = "center"
  ),

div(style = "padding: 25px;"),

 
  # script for sending key presses. Currently this just sends return key presses
  tags$script('
    $(document).on("keypress", function (e) {
            if (e.which == 13) {
              Shiny.onInputChange("keysPressed", e.which);
            } else {
              Shiny.onInputChange("keysPressed", 0);
            }
            });
    ')
  ))

