/* global exports */
"use strict";

// module Graphics.D3.Util

exports.min           = min
exports.min$prime     = min$prime
exports.max           = max
exports.max$prime     = max$prime
exports.extent        = extent
exports.extent$prime  = extent$prime


function min$prime(fn, data) {
  return d3.min(data,fn);
}
function max$prime(fn, data) {
  return d3.max(data, fn);
}
function min(data) {
  return d3.min(data);
}
function max(data) {
  return d3.max(data);
}
function extent(data) {
  return d3.extent(data);
}
function extent$prime(fn, data) {
  return d3.extent(data, fn);
}
