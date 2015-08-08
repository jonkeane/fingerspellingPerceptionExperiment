shinyjs.updateVideoCache = function(params){
                    ga(function(tracker) {
                    var clientId = tracker.get("clientId");
                    Shiny.onInputChange("gaClientID", clientId);
                   });
};