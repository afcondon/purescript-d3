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
function nodes(ns, force) {
  return force.nodes(ns);
}
function links(ls, force) {
  return force.links(ls);
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
  return force.on('tick', function(d) {
                            return callback(d);
                  });
}
function onDragStart(force, callback) {
  return force.on('dragstart', function(d) {
                            return callback(d);
                  });
}

exports.forceLayout        = force
exports.linkDistanceImpl   = linkDistance
exports.linkStrengthImpl   = linkStrength
exports.frictionImpl       = friction
exports.chargeImpl         = charge
exports.chargeDistanceImpl = chargeDistance
exports.thetaImpl          = theta
exports.gravityImpl        = gravity
exports.alphaImpl          = alpha
exports.startImpl          = start
exports.resumeImpl         = resume
exports.stopImpl           = stop
exports.tickImpl           = tick

function force() {
  return d3.layout.force();
}

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
