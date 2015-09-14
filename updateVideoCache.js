Shiny.addCustomMessageHandler('updateVideoCache',
function(params){
Shiny.onInputChange("donePrecaching", false);

function pausecomp(millis)
 {
  var date = new Date();
  var curDate = null;
  do { curDate = new Date(); }
  while(curDate-date < millis);
}

var videosToCache = params;
for(var i=0; i < videosToCache.length; i++){

if(videosToCache[i] !== null){
  myLoader.addFiles(videosToCache[i].concat("*:",videosToCache[i]));
} else {
  console.log("the param  was null");
}
}

// myLoader.on("finish", function(){  console.log("All assets loaded."); });
// myLoader.on("finish", function(){ Shiny.onInputChange("donePrecaching", 1); });
});