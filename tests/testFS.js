module.exports = {
  'Test captcha' : function (browser) {
    browser
      .url('http://shiny.jonkeane.com/fingerspelling/')
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
    // Cannot see select elements?
    // .waitForElementVisible('select#hearingStatus', 1000)
	  // .setValue('select#hearingStatus', 'deaf')
      .waitForElementVisible('input#ageAcqASL', 3000)
      .assert.containsText('body', 'Language background')
	    .setValue('input#ageAcqASL', '1')
      .waitForElementVisible('input#age', 1000)
	    .setValue('input#age', '1')
      .waitForElementVisible('input#howASL', 1000)
	    .setValue('input#howASL', 'computers')
      .waitForElementVisible('input#nativeLang', 1000)
      .setValue('input#nativeLang', 'javascript')
      .waitForElementVisible('input#ageAcqEng', 1000)
	    .setValue('input#ageAcqEng', '1')
      .waitForElementVisible('input#langs', 1000)
	    .setValue('input#langs', 'javascript	')

      // .waitForElementVisible('select#interpreter', 1000)
      // .setValue('select#interpreter', 'no')
      .waitForElementVisible('input#cert', 1000)
	    .setValue('input#cert', 'Illinois Certified')
      .waitForElementVisible('input#howLongCert', 1000)
	    .setValue('input#howLongCert', '1 year')

      .waitForElementVisible('input#fatherNativeLang', 1000)
	    .setValue('input#fatherNativeLang', 'FORTRAN')
      .waitForElementVisible('input#fatherLangs', 1000)
	    .setValue('input#fatherLangs', 'FORTRAN, C')
      .waitForElementVisible('input#fatherWhenLearnEnglish', 1000)
	    .setValue('input#fatherWhenLearnEnglish', '0')

      .waitForElementVisible('input#motherNativeLang', 1000)
	    .setValue('input#motherNativeLang', 'FORTRAN')
      .waitForElementVisible('input#motherLangs', 1000)
	    .setValue('input#motherLangs', 'FORTRAN, C')
      .waitForElementVisible('input#motherWhenLearnEnglish', 1000)
	    .setValue('input#motherWhenLearnEnglish', '0')

      .pause(500) // for safari
      .waitForElementVisible('button#languageBGSubmit', 1000)
      .click('button#languageBGSubmit')
      .pause(1000)
      .waitForElementVisible('button#continue', 10000)
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
