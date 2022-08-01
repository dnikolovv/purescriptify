"use strict";

const React = require("react");

exports.mergeStyles = (styles) =>
  Object.assign.apply(null, [{}].concat(styles));

exports.unsafeCreateDOMComponent_ = (createElement) => (el) => {
  const flattenDataProp = (props, ref) => {
    var data = null;
    if (props._data != null) {
      data = { _data: undefined };
      Object.entries(props._data).forEach(function (entry) {
        data["data-" + entry[0]] = entry[1];
      });
    }
    var aria = null;
    if (props._aria != null) {
      aria = { _aria: undefined };
      Object.entries(props._aria).forEach(function (entry) {
        aria["aria-" + entry[0]] = entry[1];
      });
    }
    return Object.assign({ ref }, props, data, aria);
  };
  return () => {
    const c = React.forwardRef((props, ref) =>
      createElement(el)(flattenDataProp(props, ref))
    );
    c.displayName = el;
    return c;
  };
};
