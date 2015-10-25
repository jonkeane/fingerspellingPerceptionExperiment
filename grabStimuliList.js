Shiny.addCustomMessageHandler('grabStimuliList',
function(params){
console.log(params);
Shiny.onInputChange("grabStims", 1);
});

