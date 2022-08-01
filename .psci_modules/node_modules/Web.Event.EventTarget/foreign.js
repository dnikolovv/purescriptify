"use strict";

exports.eventListener = function (fn) {
  return function () {
    return function (event) {
      return fn(event)();
    };
  };
};

exports.addEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, useCapture);
        };
      };
    };
  };
};

exports.removeEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          return target.removeEventListener(type, listener, useCapture);
        };
      };
    };
  };
};

exports.dispatchEvent = function (event) {
  return function (target) {
    return function () {
      return target.dispatchEvent(event);
    };
  };
};
