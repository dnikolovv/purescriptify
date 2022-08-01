"use strict";

/* eslint-env node*/

exports.unsafeFromNullable = function unsafeFromNullable(msg) {
  return function (x) {
    if (x === null) throw new Error(msg);
    return x;
  };
};

exports.spawnImpl = function spawnImpl(command) {
  return function (args) {
    return function (opts) {
      return function () {
        return require("child_process").spawn(command, args, opts);
      };
    };
  };
};

exports.execImpl = function execImpl(command) {
  return function (opts) {
    return function (callback) {
      return function () {
        return require("child_process").exec(
          command,
          opts,
          function (err, stdout, stderr) {
            callback(err)(stdout)(stderr)();
          }
        );
      };
    };
  };
};

exports.execFileImpl = function execImpl(command) {
  return function (args) {
    return function (opts) {
      return function (callback) {
        return function () {
          return require("child_process").execFile(
            command,
            args,
            opts,
            function (err, stdout, stderr) {
              callback(err)(stdout)(stderr)();
            }
          );
        };
      };
    };
  };
};

exports.execSyncImpl = function execSyncImpl(command) {
  return function (opts) {
    return function () {
      return require("child_process").execSync(command, opts);
    };
  };
};

exports.execFileSyncImpl = function execFileSyncImpl(command) {
  return function (args) {
    return function (opts) {
      return function () {
        return require("child_process").execFileSync(command, args, opts);
      };
    };
  };
};

exports.fork = function fork(cmd) {
  return function (args) {
    return function () {
      return require("child_process").fork(cmd, args);
    };
  };
};

exports.mkOnExit = function mkOnExit(mkChildExit) {
  return function onExit(cp) {
    return function (cb) {
      return function () {
        cp.on("exit", function (code, signal) {
          cb(mkChildExit(code)(signal))();
        });
      };
    };
  };
};

exports.mkOnClose = function mkOnClose(mkChildExit) {
  return function onClose(cp) {
    return function (cb) {
      return function () {
        cp.on("close", function (code, signal) {
          cb(mkChildExit(code)(signal))();
        });
      };
    };
  };
};

exports.onDisconnect = function onDisconnect(cp) {
  return function (cb) {
    return function () {
      cp.on("disconnect", cb);
    };
  };
};

exports.mkOnMessage = function mkOnMessage(nothing) {
  return function (just) {
    return function onMessage(cp) {
      return function (cb) {
        return function () {
          cp.on("message", function (mess, sendHandle) {
            cb(mess, sendHandle ? just(sendHandle) : nothing)();
          });
        };
      };
    };
  };
};

exports.onError = function onError(cp) {
  return function (cb) {
    return function () {
      cp.on("error", function (err) {
        cb(err)();
      });
    };
  };
};

exports.undefined = undefined;
exports.process = process;
