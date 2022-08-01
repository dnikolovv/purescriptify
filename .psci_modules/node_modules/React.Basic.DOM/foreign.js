"use strict";

const ReactDOM = require("react-dom");

exports.renderThen = (jsx) => (node) => (callback) => () =>
  ReactDOM.render(jsx, node, callback);

exports.hydrateThen = (jsx) => (node) => (callback) => () =>
  ReactDOM.hydrate(jsx, node, callback);

exports.unmount = (node) => () => ReactDOM.unmountComponentAtNode(node);

exports.createPortal = (jsx) => (node) => ReactDOM.createPortal(jsx, node);
