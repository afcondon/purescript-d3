/* global exports */
"use strict";

// module Graphics.D3.Util

exports.min       = min
exports.minFn     = minFn
exports.max       = max
exports.maxFn     = maxFn
exports.extent    = extent
exports.extentFn  = extentFn


function minFn(fn, data) {
  return d3.min(data,fn);
}
function maxFn(fn, data) {
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
function extentFn(fn, data) {
  return d3.extent(data, fn);
}
