/* eslint-disable no-eq-null, eqeqeq */

"use strict";

exports["null"] = null;

exports.nullable = function (a, r, f) {
  return a == null ? r : f(a);
};

exports.notNull = function (x) {
  return x;
};
