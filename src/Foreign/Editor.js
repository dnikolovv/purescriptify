var Editor = require('react-simple-code-editor')
var {highlight, languages} = require("prismjs/components/prism-core");
var _ = require('prismjs/components/prism-haskell');
var _ = require('prismjs/components/prism-purescript');
var _ = require('prismjs/components/prism-markup');
var _ = require('prismjs/themes/prism.css');

exports.editor_ = Editor.default;
exports.highlight_ = highlight;
exports.languages_ = languages;

console.log(languages);