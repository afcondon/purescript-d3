/* global exports */
"use strict";

// module Graphics.D3.Behavior.Zoom

exports.createZoomableImpl = d3.behavior.zoom
exports.onZoomImpl         = attachZoomCallback

// functions that attach event handlers
function attachZoomCallback(zoomable, handler) {
  zoomable.on("zoom", handler);
  return zoomable;
}
