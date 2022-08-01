"use strict";

exports.process = process;

exports.onBeforeExit = function (callback) {
  return function () {
    process.on("beforeExit", callback);
  };
};

exports.onExit = function (callback) {
  return function () {
    process.on("exit", function (code) {
      callback(code)();
    });
  };
};

exports.onUncaughtException = function (callback) {
  return function () {
    process.on("uncaughtException", function (error) {
      callback(error)();
    });
  };
};

exports.onUnhandledRejection = function (callback) {
  return function () {
    process.on("unhandledRejection", function (error, promise) {
      callback(error)(promise)();
    });
  };
};

exports.onSignalImpl = function (signal) {
  return function (callback) {
    return function () {
      process.on(signal, callback);
    };
  };
};

exports.chdir = function (dir) {
  return function () {
    process.chdir(dir);
  };
};

exports.setEnv = function (var_) {
  return function (val) {
    return function () {
      process.env[var_] = val;
    };
  };
};

exports.unsetEnv = function (var_) {
  return function () {
    delete process.env[var_];
  };
};

exports.exit = function (code) {
  return function () {
    process.exit(code);
  };
};

exports.copyArray = function (xs) {
  return function () {
    return xs.slice();
  };
};

exports.copyObject = function (o) {
  return function () {
    return Object.assign({}, o);
  };
};
