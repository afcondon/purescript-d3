/* global exports */
"use strict";

// module Graphics.D3.Layout.Force

// for typeclass forceGraphLayout
exports.sizeImpl            = size
exports.nodesImpl           = nodes
exports.linksImpl           = links

function size(wh, force) {
  return force.size(wh);
}
function nodes(nodes, force) {
  return force.nodes(nodes);
}
return nodes(links, force) {
  return force.links(links);
}

exports.onTickImpl      = onTick
exports.onDragStartImpl = onDragStart
exports.dragImpl        = drag
exports.createDragImpl  = createDrag

function drag(force) {
  return force.drag();
}
function createDrag(obj, callable) {
  return callable.call(obj);
}
function onTick(callback, force) {
  return force.on('tick',
                  function(d) { return callback(d)(); })
}
function onDragStart(callback, force) {
  return force.on('dragstart', callback })
}

exports.forceLayout         = d3.layout.force;
exports.linkDistanceImpml   = linkDistance
exports.linkStrengthImpml   = linkStrength
exports.frictionImpml       = friction
exports.chargeImpml         = charge
exports.chargeDistanceImpml = chargeDistance
exports.thetaImpml          = theta
exports.gravityImpml        = gravity
exports.alphaImpml          = alpha
exports.startImpml          = start
exports.resumeImpml         = resume
exports.stopImpml           = stop
exports.tickImpml           = tick


function linkDistance(distance, force) {
  return force.linkDistance(distance);
}

function linkStrength(strength, force) {
  return force.linkStrength(strength);
}

function friction(friction, force) {
  return force.friction(friction);
}

function charge(charge, force) {
  return force.charge(charge);
}

function chargeDistance(distance, force) {
  return force.chargeDistance(distance);
}

function theta(theta, force) {
  return force.theta(theta);
}

function gravity(gravity, force) {
  return force.gravity(gravity);
}

function alpha(alpha, force) {
  return force.alpha(alpha);
}

function start(force) {
  return force.start();
}

function resume(force) {
  return force.resume();
}

function stop(force) {
  return force.stop();
}

function tick(force) {
  return force.tick();
}
