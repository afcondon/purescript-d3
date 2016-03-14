/* global exports */
"use strict";

// module Graphics.D3.Scale

exports.exponentImpl          = exponent
exports.baseImpl              = base
exports.quantilesImpl         = quantiles
exports.rangePointsImpl       = rangePoints
exports.rangeBandsImpl        = rangeBands
exports.rangeRoundBandsImpl   = rangeRoundBands
exports.rangeBandImpl         = rangeBand
exports.rangeExtentImpl       = rangeExtent
exports.unsafeInvertImpl      = invert
exports.unsafeRangeRoundImpl  = rangeRound
exports.unsafeInterpolateImpl = interpolate
exports.unsafeClampImpl       = clamp
exports.unsafeNiceImplC       = niceWithCount
exports.unsafeNiceImpl        = nice
exports.unsafeGetTicksC       = ticksWithCount
exports.unsafeGetTicks        = ticksDefault
exports.unsafeGetTickFormatC  = tickFormat
exports.unsafeGetTickFormat   = tickFormatDefault

exports.linearScale         = d3.scale.linear;
exports.powerScale          = d3.scale.pow;
exports.sqrtScale           = d3.scale.sqrt;
exports.quantizeScale       = d3.scale.quantize;
exports.quantileScale       = d3.scale.quantile;
exports.thresholdScale      = d3.scale.threshold;
exports.ordinalScale        = d3.scale.ordinal;

exports.logScale = function() {  // why is this different from others? TODO
    return d3.scale.log();
};

function niceWithCount(count, scale) {
  return scale.nice(count);
}
function nice(scale) {
  return scale.nice();
}
function ticksWithCount(count, scale) { // don't think D3 supports this TODO
  return scale.ticks(count);
}
function ticksDefault(scale) {
  return scale.ticks();
}
function tickFormat(count, format, scale) {
  return scale.ticks(count, format);
}
function tickFormatDefault(count, scale) {
  return scale.tickFormat(count);
}


function invert(scale) {
  return scale.copy().invert;       // function brackets missing?? check this TODO
}
function rangeRound(values, scale) {
  return scale.rangeRound(values);
}
function interpolate(factory, scale) {
  return scale.interpolate(factory);
}
function clamp(bool, scale) {
  return scale.clamp(bool);
}

function exponent(k, scale) {
  return scale.exponent(k);
}
function base(base, scale) {
  return scale.base(base);
}
function quantiles(scale) {
  return scale.quantiles();
}
function rangePoints(min, max, padding, ordinalScale) {
  return ordinalScale.rangePoints([min,max], padding);
}
function rangeBands(min, max, padding, outerPadding, ordinalScale) {
  return ordinalScale.rangeBands([min,max], padding, outerPadding);
}
function rangeRoundBands(min, max, padding, outerPadding, ordinalScale) {
  return ordinalScale.rangeRoundBands([min,max], padding, outerPadding);
}
function rangeBand(ordinalScale) {
  return ordinalScale.rangeBand();
}
function rangeExtent(ordinalScale) {
  return ordinalScale.rangeExtent();
}
