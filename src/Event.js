/* global exports */
"use strict";

// module Graphics.D3.Event
// var Data_Maybe = require("../Data.Maybe");
var Data_Maybe = PS["Data.Maybe"];

// for typeclass forceGraphLayout
exports.currentD3Event = function() {
  var possibleEvent = d3.event;
  if (possibleEvent !== null) {
    if(typeof possibleEvent.sourceEvent !== 'undefined') {
      // console.log("possibleEvent.sourceEvent is" + possibleEvent.sourceEvent);
      return new Data_Maybe.Just(possibleEvent.sourceEvent);
    }
  }
  return new Data_Maybe.Nothing();
}
