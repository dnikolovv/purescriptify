"use strict";

exports["new"] = function () {
  return new FormData();
};

exports._fromFormElement = function(form) {
  return new FormData(form);
};

exports._append = function (name, value, fd) {
  fd.append(name, value);
};

exports._appendBlob = function (name, value, filename, fd) {
  fd.append(name, value, filename);
};

exports._delete = function (name, fd) {
  fd.delete(name);
};

exports._has = function (name, fd) {
  return fd.has(name);
};

exports._set = function (name, value, fd) {
  fd.set(name, value);
};

exports._setBlob = function (name, value, filename, fd) {
  fd.set(name, value, filename);
};
