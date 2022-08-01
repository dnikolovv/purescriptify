/* globals exports */
"use strict";

exports.nan = NaN;

exports.isNaN = isNaN;

exports.infinity = Infinity;

exports.isFinite = isFinite;

exports.fromStringImpl = function(str, isFinite, just, nothing) {
  var num = parseFloat(str);
  if (isFinite(num)) {
    return just(num);
  } else {
    return nothing;
  }
};
