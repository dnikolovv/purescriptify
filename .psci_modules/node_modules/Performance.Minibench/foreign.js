"use strict";

exports.hrTime = process.hrtime;

exports.gc = function() {
  global.gc && global.gc();
};

exports.toFixed = function(n) {
  return n.toFixed(2);
};
