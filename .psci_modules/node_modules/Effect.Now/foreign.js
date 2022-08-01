"use strict";

exports.now = function () {
  return Date.now();
};

exports.getTimezoneOffset = function () {
  var n = new Date(Date.now());
  return n.getTimezoneOffset();
};
