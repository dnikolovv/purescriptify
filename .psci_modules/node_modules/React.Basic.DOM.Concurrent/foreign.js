"use strict";

const ReactDOM = require("react-dom");
const createRoot = ReactDOM.createRoot || ReactDOM.unstable_createRoot;
const createBlockingRoot =
  ReactDOM.createBlockingRoot || ReactDOM.unstable_createBlockingRoot;

exports.createRoot = (element) => () => createRoot(element);

exports.createBlockingRoot = (element) => () => createBlockingRoot(element);

exports.renderRoot = (root) => (jsx) => () => root.render(jsx);
