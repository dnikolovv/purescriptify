/* global Buffer */
"use strict";

exports.copyAll = function (a) {
  return function () {
    return Buffer.from(a);
  };
};

exports.writeInternal = function (ty) {
  return function (value) {
    return function (offset) {
      return function (buf) {
        return function () {
          buf["write" + ty](value, offset);
        };
      };
    };
  };
};

exports.writeStringInternal = function (encoding) {
  return function (offset) {
    return function (length) {
      return function (value) {
        return function (buff) {
          return function () {
            return buff.write(value, offset, length, encoding);
          };
        };
      };
    };
  };
};

exports.setAtOffset = function (value) {
  return function (offset) {
    return function (buff) {
      return function () {
        buff[offset] = value;
      };
    };
  };
};

exports.copy = function (srcStart) {
  return function (srcEnd) {
    return function (src) {
      return function (targStart) {
        return function (targ) {
          return function () {
            return src.copy(targ, targStart, srcStart, srcEnd);
          };
        };
      };
    };
  };
};

exports.fill = function (octet) {
  return function (start) {
    return function (end) {
      return function (buf) {
        return function () {
          buf.fill(octet, start, end);
        };
      };
    };
  };
};
