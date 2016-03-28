/* global exports */
"use strict";

// module Graphics.D3.Selection

exports.selectImpl        = select
exports.filterImpl        = filter
exports.selectElementImpl = selectElement
exports.selectAllImpl     = selectAll
exports.rootSelectImpl    = rootSelect
exports.rootSelectAllImpl = rootSelectAll
exports.unsafeRemoveImpl  = unsafeRemove
exports.bindDataImpl      = bindData
exports.bindDataImplN     = bindDataWithKeyFn
exports.bindDataImplS     = bindDataWithKeyFn
exports.orderImpl         = order
exports.enterImpl         = enter
exports.exitImpl          = exit
exports.transitionImpl    = transition
exports.unsafeAppendImpl  = unsafeAppend
exports.unsafeAttrImpl    = unsafeAttr
exports.unsafeAttrImplP   = unsafeAttrP
exports.unsafeAttrImplPP  = unsafeAttrPP
exports.unsafeClassedImpl = unsafeClassed
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
function select(selector, selection) {
  return selection.select(selector);
}
function filter(selector, selection) {
  return selection.filter(selector);
}
function selectElement(element) {
  return d3.select(element);
}
function bindData(array, selection) {
  return selection.data(array);
}
function bindDataWithKeyFn(array, keyFn, selection) {
  return selection.data(array, keyFn);
}
function order(selection) {
  return selection.order();
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
function unsafeClassed(classname, val, selection) {
  return selection.classed(classname, val);
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
function attachCallbackS(selection, callback) {
  selection.on("click", callback);
  return selection;
}
function attachCallbackD(selection, callback) {
  selection.on("dblclick", callback);
  return selection;
}
