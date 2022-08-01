"use strict";

exports.createObjectURL = function (blob) {
  return function () {
    return URL.createObjectURL(blob);
  };
};

exports.revokeObjectURL = function (url) {
  return function () {
    URL.revokeObjectURL(url);
  };
};
