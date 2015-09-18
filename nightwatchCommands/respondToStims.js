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
							  .waitForElementPresent('input#word', 1000)
							  .setValue('input#word[type=text]', 'nightwatch test')

						    .waitForElementVisible('button#submit', 1000)
						    .pause(500) // for safari
						    .click('button#submit')

								.pause(1000);
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
