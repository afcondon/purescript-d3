/* global exports */
"use strict";

// module Graphics.D3.EffFnExtra
// var Data_Tuple = require("../Data.Tuple");
var Data_Tuple = PS["Data.Tuple"];

// custom version of mkEffFn1 which passes a Tuple of both datum _and_ 'this'
// enables callbacks in the D3 style which rely on 'this' for access to the
// D3Element associated with the datum
exports.mkCallbackWithT = function mkCallbackWithT(fn) {
  return function(x) {
    var d3Tuple = new Data_Tuple.Tuple(x, this);
    return fn(d3Tuple)();
  };
};

// another callback-making function, this time taking a property name and bundling that with
// callback params too
exports.mkCallbackWithProp = function mkCallbackWithProp(fn) {
  return function(propName) {
    return function(d) {
      var cbParams = { datum: d, elem: this
                     , prop: this[propName]
                     , timestamp: d3.event.timeStamp
                     , meta: d3.event.metaKey
                     , shift: d3.event.shiftKey
                     , ctrl: d3.event.ctrlKey
                     , alt: d3.event.altKey };
      return fn(cbParams)();
    }
  };
};
