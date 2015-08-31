shinyjs.changeVideo = function(params){

function contains(a, obj) {
    for (var i = 0; i < a.length; i++) {
if (a[i] === obj) {
return true;
}
}
return false;
}

// focus the input box
document.getElementById("word").focus();

var stimDiv = document.getElementById("stimuliVideo");
if(stimDiv !== null && stimDiv.childNodes.length > 0) {
 stimDiv.removeChild(stimDiv.childNodes[0]);
}

if(stimDiv === null){
  console.log("Error: Cannotfind the stim div.");
}

var inCache = myLoader.files;
var idsInCache = [];
for (var i=0;i<inCache.length;i++){idsInCache = idsInCache.concat(inCache[i].id);}

var stim;

// Check if the video is cached, if it is, use the cache, if not, load directly with a warning.
if(contains(idsInCache, params.video)){
// set video parameters for the video up next
// myLoader.getFile(params.video).autoplay = true;
// myLoader.getFile(params.video).nodeType = "video/mp4"; // doesnt work? 
  videoFromCache = myLoader.getFile(params.video);
  try{
   stim = stimDiv.appendChild(videoFromCache);
  } catch (e) {
    console.log(e)
    console.log("There was an error loading the video from the cache, instead, I'm fetching the video directly.");
    var vid = document.createElement("video");
    vid.setAttribute("type", "video/mp4");
    vid.setAttribute("preload", "auto");
    vid.setAttribute("src", params.video);
    // vid.autoplay = true;
  
    stim = stimDiv.appendChild(vid);   
  }
  myLoader.removeFile(params.video);
  stim.setAttribute("type", "video/mp4");
} else {
  console.log("Error: The file was not in the cache!");
  console.log(params.video);

  var vid = document.createElement("video");
  vid.setAttribute("type", "video/mp4");
  vid.setAttribute("preload", "auto");
  vid.setAttribute("src", params.video);
  // vid.autoplay = true;

  stim = stimDiv.appendChild(vid);
}

// replay the video so the total views are 2. This should be pushed up to be extracted from params.
var playCount = 0;
stim.play();
stim.onended = function () {
  playCount += 1;
  if(playCount < params.rep){
    stim.play();
  }
};

};