"use strict";

exports._xmlHttpRequest = function (respType) {
  var xhr = new XMLHttpRequest();
  xhr.responseType = respType;
  return xhr;
};

exports._abort = function (xhr) {
  xhr.abort();
};

exports._getAllResponseHeaders = function (xhr) {
  return xhr.getAllResponseHeaders();
};

exports._getResponseHeader = function (header, xhr) {
  return xhr.getResponseHeader(header);
};

exports._open = function (method, url, username, password, xhr) {
  xhr.open(method, url, true, username, password);
};

exports._overrideMimeType = function (mimeType, xhr) {
  xhr.overrideMimeType(mimeType);
};

exports._send = function (payload, xhr) {
  xhr.send(payload);
};

exports._setRequestHeader = function (header, value, xhr) {
  xhr.setRequestHeader(header, value);
};

exports._setProperty = function (prop, value, xhr) {
  xhr[prop] = value;
};

exports._getProperty = function (prop, xhr) {
  return xhr[prop];
};
