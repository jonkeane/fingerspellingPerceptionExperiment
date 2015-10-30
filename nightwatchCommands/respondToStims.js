exports.command = function(reps, callback) {
	var self = this;

	var i=0;

	var wordInput = function(result){
						// console.log(result);
				        if (result.value && result.value.indexOf('What word was fingerspelled?') > -1) {
				        	// Element is present, do the appropriate tests
							// console.log("keep looping!");
							this
							  // .frame(null)
								.waitForElementPresent('video', 1000)
								.waitForElementPresent('input#word', 1000)

								.getAttribute('video', 'src', function (res) {
									// grab and digest the stimuli file name.
									var str = res.value;
									var res = str.split("/");
									var currStim = res[res.length - 1];
									var currStimAns = this.globals.stimAns[currStim];
									this.setValue('input#word[type=text]', currStimAns);
								})

								.WaitForAttribute('button#submit', 'disabled', function(attrib) {
										// console.log(attrib);
										return attrib === null;
								}, 1000)
						    .pause(1000) // for safari
						    .click('button#submit')

								.pause(5000); // long, for waiting for videos to complete.
								i++;
								// console.log(i);
				        } else {
				        }
						this.ifElementVisible('label[for=word]', 1000, false, checkElem);
					};

	var checkElem = function(result){
		// console.log(result);
		        if (result.value) {
					this.getText('label[for=word]', wordInput);
		        } else {
		            // Element is not present.
					// console.log("Element isn't present, break!");
					// add this i to testing?
					console.log(i);
					return;
		        }
		    };


    this.ifElementVisible('label[for=word]', 1000, false, checkElem);


	if( typeof callback === "function"){
	    callback.call(self);
	}
	return this; // allows the command to be chained.
};
