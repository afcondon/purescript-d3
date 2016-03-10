/* global exports */
"use strict";

// module Graphics.D3.SVG.Axis

exports.axis = d3.svg.axis;

// foreign import scaleImpl :: forall eff. (Scale s) = EffFn2 (d3::D3|eff) (s d Number) Axis Axis
exports.scaleImpl = scale
// foreign import orientImpl :: forall eff. EffFn2 (d3::D3|eff) String Axis Axis
exports.orientImpl = orient
// foreign import ticksImpl :: forall eff. EffFn2 (d3::D3|eff) Number Axis Axis
exports.ticksImpl = ticks
// foreign import tickFormatImpl :: forall eff. (d3::D3|eff) String Axis Axis
exports.tickFormatImpl = ticksFormat
// foreign import renderAxisImpl :: forall eff s d. (Existing s) => EffFn2 Axis (s d)
exports.renderAxisImpl = renderAxis

function scale(s, axis) {
  axis.scale(s);
  return axis;
}
function orient(orientation, axis) {
  axis.orient(orientation);
  return axis;
}
function ticks(count, axis) {
  axis.ticks(count);
  return axis;
}
function tickFormat(format, axis) {
  axis.tickFormat(d3.format(format));
  return axis;
}
function renderAxis(axis, selection) {
  selection.call(axis);
  return selection;
}
