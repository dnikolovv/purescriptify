"use strict";

// A helper which transforms the result ofencodeURIComponent to be compliant
// with RFC3896, as described in the MDN documentation here:
//
// https://web.archive.org/web/20201206001047/https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
function toRFC3896(input) {
  return input.replace(/[!'()*]/g, function (c) {
    return "%" + c.charCodeAt(0).toString(16);
  });
}

exports._encodeURIComponent = function encode(fail, succeed, input) {
  try {
    return succeed(toRFC3896(encodeURIComponent(input)));
  } catch (err) {
    return fail(err);
  }
};

exports._encodeFormURLComponent = function encode(fail, succeed, input) {
  try {
    return succeed(toRFC3896(encodeURIComponent(input)).replace(/%20/g, "+"));
  } catch (err) {
    return fail(err);
  }
};

function _decodeURIComponent(fail, succeed, input) {
  try {
    return succeed(decodeURIComponent(input));
  } catch (err) {
    return fail(err);
  }
}

exports._decodeURIComponent = _decodeURIComponent;

exports._decodeFormURLComponent = function encode(fail, succeed, input) {
  return _decodeURIComponent(fail, succeed, input.replace(/\+/g, " "));
};
