/* global exports */
"use strict";

// module Graphics.D3.EffFnThis
var Data_Tuple = require("../Data.Tuple");

// custom / temporary hack of mkEffFn1 to prove that "this" pointer can be passed thru to callback
exports.mkEffFnThis1 = function mkEffFnThis1(fn) {
  return function(x) {
    return fn(this)();
  };
};

exports.mkEffFnTuple1 = function mkEffFnTuple1(fn) {
  return function(x) {
    var d3Tuple = new Data_Tuple.Tuple(x, this);
    return fn(d3Tuple)();
  };
};
