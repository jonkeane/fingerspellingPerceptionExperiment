// from https://github.com/nightwatchjs/nightwatch/issues/121#issuecomment-39988409

var util = require('util');
var drequire = require('./_drequire.js');
var WaitForElementVisible = drequire('waitForElementVisible.js');



function ifElementVisible() {
  WaitForElementVisible.call(this);
}

util.inherits(ifElementVisible, WaitForElementVisible);

ifElementVisible.prototype.fail = function (result, actual, expected, defaultMsg) {
  this.message = this.formatMessage(defaultMsg);

  // If we're not aborting on failure, mark this test case as passed.
  // That way, we don't fail the test case on an if statement
  if(this.abortOnFailure)
    this.client.assertion(false, actual, expected, this.message, this.abortOnFailure);
  else
    this.client.assertion(true, actual, expected, this.message + " - not critical", this.abortOnFailure);

  return this.complete(result);
}

module.exports = ifElementVisible;