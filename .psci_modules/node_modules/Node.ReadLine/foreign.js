"use strict";

// module Node.ReadLine

exports.createInterfaceImpl = function (options) {
  return function () {
    var readline = require("readline");
    return readline.createInterface({
      input: options.input,
      output: options.output,
      completer:
        options.completer &&
        function (line) {
          var res = options.completer(line)();
          return [res.completions, res.matched];
        },
      terminal: options.terminal,
      historySize: options.historySize,
    });
  };
};

exports.close = function (readline) {
  return function () {
    readline.close();
  };
};

exports.prompt = function (readline) {
  return function () {
    readline.prompt();
  };
};

exports.question = function (text) {
  return function (callback) {
    return function (readline) {
      return function () {
        readline.question(text, function (result) {
          callback(result)();
        });
      };
    };
  };
};

exports.setPrompt = function (prompt) {
  return function (readline) {
    return function () {
      readline.setPrompt(prompt);
    };
  };
};

exports.setLineHandler = function (callback) {
  return function (readline) {
    return function () {
      readline.removeAllListeners("line");
      readline.on("line", function (line) {
        callback(line)();
      });
    };
  };
};
