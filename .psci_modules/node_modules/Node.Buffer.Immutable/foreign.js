/* global Buffer */
"use strict";

exports.showImpl = require("util").inspect;

exports.eqImpl = function (a) {
  return function (b) {
    return a.equals(b);
  };
};

exports.compareImpl = function (a) {
  return function (b) {
    return a.compare(b);
  };
};

exports.create = function (size) {
  return Buffer.alloc(size);
};

exports.fromArray = function (octets) {
  return Buffer.from(octets);
};

exports.size = function (buff) {
  return buff.length;
};

exports.toArray = function (buff) {
  var json = buff.toJSON();
  return json.data || json;
};

exports.toArrayBuffer = function (buff) {
  return buff.buffer.slice(buff.byteOffset, buff.byteOffset + buff.byteLength);
};

exports.fromArrayBuffer = function (ab) {
  return Buffer.from(ab);
};

exports.fromStringImpl = function (str) {
  return function (encoding) {
    return Buffer.from(str, encoding);
  };
};

exports.readImpl = function (ty) {
  return function (offset) {
    return function (buf) {
      return buf["read" + ty](offset);
    };
  };
};

exports.readStringImpl = function (enc) {
  return function (start) {
    return function (end) {
      return function (buff) {
        return buff.toString(enc, start, end);
      };
    };
  };
};

exports.getAtOffsetImpl = function (just) {
  return function (nothing) {
    return function (offset) {
      return function (buff) {
        var octet = buff[offset];
        return octet == null ? nothing : just(octet);
      };
    };
  };
};

exports.toStringImpl = function (enc) {
  return function (buff) {
    return buff.toString(enc);
  };
};

exports.slice = function (start) {
  return function (end) {
    return function (buff) {
      return buff.slice(start, end);
    };
  };
};

exports.concat = function (buffs) {
  return Buffer.concat(buffs);
};

exports.concatToLength = function (buffs) {
  return function (totalLength) {
    return Buffer.concat(buffs, totalLength);
  };
};
