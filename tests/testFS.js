module.exports = {
  'Test captcha' : function (browser) {
    browser
      // .url('http://shiny.jonkeane.com/asl3fingerspelling/')
      .url('http://localhost:3788')
      .waitForElementVisible('input.form-control.shiny-bound-input', 4000)
      // find the captcha response elements, and fill in based on the JSON in globals.
      .elements('css selector', 'input.form-control.shiny-bound-input', function (result) {
        result.value.map(function (v, k) {
          browser.elementIdAttribute(v.ELEMENT, 'id', function(res) {
            var ans = res.value;
            browser.elementIdValue(v.ELEMENT, browser.globals.captchASLans[ans][0]);
          });
        });
      })
      .pause(1000) // for safari
      .WaitForAttribute('button#robotSubmit', 'disabled', function(attrib) {
          // console.log(attrib);
          return attrib === null;
      }, 40000)
	    .click('button#robotSubmit')
  },

  'Test background check' : function (browser) {
    browser
      .waitForElementVisible('input#studyCode', 4000)
      .assert.containsText('body', 'Language background')
	  .setValue('input#studyCode', 'nightwatchtest')
      // .waitForElementVisible('select#hearingStatus', 1000)
	  // .setValue('select#hearingStatus', 'deaf')
      .waitForElementVisible('input#ageAcqASL', 1000)
	  .setValue('input#ageAcqASL', '1')
      .waitForElementVisible('input#age', 1000)
	  .setValue('input#age', '1')
      // .waitForElementVisible('select#majorReq', 1000)
	  // .setValue('select#majorReq', 'no')
      .waitForElementVisible('input#whyASL', 1000)
	  .setValue('input#whyASL', 'nightwatchtest')
      .waitForElementVisible('input#major', 1000)
	  .setValue('input#major', 'nightwatchtest')
      .waitForElementVisible('input#nativeLang', 1000)
	  .setValue('input#nativeLang', 'javascript')
      .waitForElementVisible('input#ageAcqEng', 1000)
	  .setValue('input#ageAcqEng', '1')
      .waitForElementVisible('input#langs', 1000)
	  .setValue('input#langs', 'javascript	')

      .pause(500) // for safari
      .waitForElementVisible('button#languageBGSubmit', 1000)
      .click('button#languageBGSubmit')
      .pause(1000)
	  .assert.containsText('body', 'Now we\'re ready to start the experiment, first let\'s try a few practice items.')

  },

  'Test practice instructions check' : function (browser) {
    browser
	  .assert.containsText('body', 'Now we\'re ready to start the experiment, first let\'s try a few practice items.')
      .pause(500) // for safari
      .waitForElementVisible('button#continue', 1000)
      .click('button#continue')
      .pause(1000)
  },

  'Test practice check' : function (browser) {
    browser
  	  .respondToStims(4)
      .pause(500)
  },

  'Test allclearA instructions check' : function (browser) {
    browser
	  .assert.containsText('body', 'Good job! Now we will begin the expeirment.')
      .pause(500) // for safari
      .waitForElementVisible('button#continue', 1000)
      .click('button#continue')
      .pause(1000)
  },

  'Test allclearA check' : function (browser) {
    browser
  	  .respondToStims(15)
      .pause(500)
  },

  'Test holdsOnly instructions check' : function (browser) {
    browser
	  .assert.containsText('body', 'Take a little break if you like.')
      .pause(500) // for safari
      .waitForElementVisible('button#continue', 1000)
      .click('button#continue')
      .pause(1000)
  },

  'Test holdsOnly check' : function (browser) {
    browser
  	  .respondToStims(30)
      .pause(500)
  },

  'Test transOnly instructions check' : function (browser) {
    browser
	  .assert.containsText('body', 'Take a little break if you like.')
      .pause(500) // for safari
      .waitForElementVisible('button#continue', 1000)
      .click('button#continue')
      .pause(1000)
  },

  'Test transOnly check' : function (browser) {
    browser
  	  .respondToStims(30)
      .pause(1000)
  },


  'Test allclearB instructions check' : function (browser) {
    browser
	  .assert.containsText('body', 'Almost done! Take a little break if you like.')
      .pause(500) // for safari
      .waitForElementVisible('button#continue', 1000)
      .click('button#continue')
      .pause(1000)
  },

  'Test allclearB check' : function (browser) {
    browser
  	  .respondToStims(15)
      .pause(1000)
  },

  'Test thanks check' : function (browser) {
    browser
	  .assert.containsText('body', 'Thanks, your response was submitted successfully!')
  },

  'Test end' : function (browser) {
  	browser
	  .end();
  }
};
