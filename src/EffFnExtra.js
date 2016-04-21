/* global exports */
"use strict";

// module Graphics.D3.EffFnExtra
// var Data_Tuple = require("../Data.Tuple");
var Data_Tuple = PS["Data.Tuple"];

// custom version of mkEffFn1 passing 'this' instead of argument (probably useless)
exports.mkEffFnThis1 = function mkEffFnThis1(fn) {
  return function(x) {
    return fn(this)();
  };
};

// custom version of mkEffFn1 which passes a Tuple of both datum _and_ 'this'
// enables callbacks in the D3 style which rely on 'this' for access to the
// D3Element associated with the datum
exports.mkEffFnTuple1 = function mkEffFnTuple1(fn) {
  return function(x) {
    var d3Tuple = new Data_Tuple.Tuple(x, this);
    return fn(d3Tuple)();
  };
};
