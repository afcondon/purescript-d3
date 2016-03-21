/* global exports */
"use strict";

// module Graphics.D3.EffFnThis

// custom / temporary hack of mkEffFn1 to prove that "this" pointer can be passed thru to callback
exports.mkEffFnThis1 = function mkEffFnThis1(fn) {
  return function(x) {
    return fn(this)();
  };
};
