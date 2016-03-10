/* global exports */
"use strict";

// module Graphics.D3.Selection

exports.rootSelectImpl = d3.select
exports.unsafeRemoveImpl = d3.selection.prototype.remove(this)

exports.logMessageImpl = logMessage

function logMessage(level, message, callback) {
  console.log(level + ": " + message);
  if (level === "HAIR ON FIRE") {
    callback("zomg")
  } else {
    callback(null);
  }
}

exports.onClickImpl = attachCallbackS
exports.onDoubleClickImpl = attachCallbackD

function attachCallbackS(element, callback) {
  element.on("click", callback);
  console.log("trying out the callback");
  callback("singleClick");
  return element;
}
function attachCallbackD(element, callback) {
  element.on("dblclick", callback);
  console.log("trying out the callback");
  callback("singleClick");
  return element;
}
