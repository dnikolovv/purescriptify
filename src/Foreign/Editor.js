import Editor from "react-simple-code-editor"
import { highlight, languages } from "prismjs/components/prism-core"
import 'prismjs/components/prism-haskell'
import 'prismjs/components/prism-purescript'
import 'prismjs/components/prism-markup'
import 'prismjs/themes/prism.css'

export {
  Editor as editor_,
  highlight as highlight_,
  languages as languages_
}