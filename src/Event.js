/* global exports */
"use strict";

// module Graphics.D3.Event
var Data_Maybe = require("../Data.Maybe");
var DOM = require("../DOM.Event.Event");

// for typeclass forceGraphLayout
exports.currentD3Event = function() {
  var possibleEvent = d3.event;
  if (possibleEvent === null) {
    return new Data_Maybe.Nothing();
  } else {
    return new Data_Maybe.Just(possibleEvent);
  }
}
