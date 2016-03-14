/* global exports */
"use strict";

// module Graphics.D3.Unsafe

exports.unsafeDomainImpl      = domain
exports.unsafeRangeImpl       = range
exports.unsafeCopyImpl        = copy
exports.unsafeToFunctionImpl  = toFunction

function domain(domain, scale) {
  return scale.domain(domain);
}

function range(values, scale) {
  return scale.range(values);
}

function copy(scale) {
  return scale.copy();
}

function toFunction(scale) {
  return scale.copy();          // is that really a D3 function? check TODO
}
