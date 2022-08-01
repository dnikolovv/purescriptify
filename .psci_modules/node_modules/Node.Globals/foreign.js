"use strict";

exports.__dirname = __dirname;
exports.__filename = __filename;
exports.unsafeRequire = require;

exports.requireResolve = function (mod) {
  return function () {
    return require.resolve(mod);
  };
};
