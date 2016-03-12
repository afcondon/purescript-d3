/* global exports */
"use strict";

// module Graphics.D3.Selection

exports.rootSelectImpl    = rootSelect
exports.unsafeRemoveImpl  = unsafeRemove
exports.rootSelectAllImpl = rootSelectAll
exports.selectAllImpl     = selectAll
exports.bindDataImpl      = bindData
exports.enterImpl         = enter
exports.exitImpl          = exit
exports.transitionImpl    = transition
exports.unsafeAppendImpl  = unsafeAppend
exports.unsafeAttrImpl    = unsafeAttr
exports.unsafeAttrImplP   = unsafeAttrP
exports.unsafeAttrImplPP  = unsafeAttrPP
exports.unsafeStyleImpl   = unsafeStyle
exports.unsafeStyleImplP  = unsafeStyleP
exports.unsafeStyleImplPP = unsafeStylePP
exports.unsafeTextImpl    = unsafeText
exports.unsafeTextImplP   = unsafeTextP
exports.unsafeTextImplPP  = unsafeTextPP
exports.delayImpl         = delay
exports.delayImplP        = delayP
exports.delayImplPP       = delayPP
exports.durationImpl      = duration
exports.durationImplP     = durationP
exports.durationImplPP    = durationPP

// event handlers
exports.onClickImpl             = attachCallbackS
exports.onDoubleClickImpl       = attachCallbackD
exports.unsafeOnClickImpl       = unsafeAttachCallbackS
exports.unsafeOnDoubleClickImpl = unsafeAttachCallbackD


// only here temporarily as a guide
exports.logMessageImpl = logMessage
function logMessage(level, message, callback) {
  console.log(level + ": " + message);
  if (level === "HAIR ON FIRE") {
    callback("zomg")
  } else {
    callback(null);
  }
}
// end
function unsafeRemove(selection) {
  d3.selection.prototype.remove(selection);
}
function rootSelect(selector) {
  return d3.select(selector);
}
function rootSelectAll(selector) {
  return d3.selectAll(selector);
}
function selectAll(selector, selection) {
  return selection.selectAll(selector);
}
function bindData(array, selection) {
  return selection.data(array);
}
function enter(update) {
  return update.enter();
}
function exit(update) {
  return update.exit();
}
function transition(selection) {
  return selection.transition();
}
function unsafeAppend(tag, selection) {
  return selection.append(tag);
}
function unsafeAttr(key, val, selection) {  // val is simple value
  return selection.attr(key, val);
}
function unsafeAttrP(key, val, selection) {  // val is (d -> v)
  return selection.attr(key, val);
}
function unsafeAttrPP(key, val, selection) { // val is (d -> Number -> v)
  return selection.attr(key, function(d,i) { return val(d)(i); })
}
function unsafeStyle(key, val, selection) {
  return selection.style(key, val);
}
function unsafeStyleP(key, val, selection) {
  return selection.style(key, val);
}
function unsafeStylePP(key, val, selection) {
  return selection.style(key, function (d, i) { return val(d)(i); })
}
function unsafeText(text, selection) {
  return selection.text(text);
}
function unsafeTextP(text, selection) {
  return selection.text(text);
}
function unsafeTextPP(text, selection) {
  return selection.text(function (d,i) { return text(d)(i); });
}
function delay(delay, transition) {
  return transition.delay(delay);
}
function delayP(delay, transition) {
  return transition.delay(delay);
}
function delayPP(delay, transition) {
  return transition.delay(function (d,i) { return delay(d)(i); });
}
function duration(duration, transition) {
  return transition.duration(duration);
}
function durationP(duration, transition) {
  return transition.duration(duration);
}
function durationPP(duration, transition) {
  return transition.duration(function (d, i) { return duration(d)(i); });
}

// functions that attach event handlers
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
function unsafeAttachCallbackS(callback, clickable) {
  clickable.on("click", function(d) { callback(d)(); });
  console.log("trying out the callback");
  callback("singleClick");
  return clickable;
}
function unsafeAttachCallbackD(callback, clickable) {
  clickable.on("dblclick", function(d) { callback(d)(); });
  console.log("trying out the callback");
  callback("doubleClick");
  return clickable;
}
