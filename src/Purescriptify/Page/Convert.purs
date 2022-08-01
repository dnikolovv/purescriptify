module Purescriptify.Page.Convert where

import Prelude
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Editor (EditorLanguage(..), editor)
import Purescriptify.Converter (convert, mkHtmlInput, unConvertedModule, unHtmlInput)
import React.Basic.DOM as R
import React.Basic.Hooks as React

mkConverter :: Effect (Unit -> React.JSX)
mkConverter =
  React.component "Converter" \_ -> React.do
    (htmlInput /\ setHtmlInput) <- React.useState initialInput
    pure
      $ R.section
          { className: "w-full h-screen grid grid-cols-2"
          , children:
              [ editor
                  { value: unHtmlInput htmlInput
                  , onValueChange: (\c -> setHtmlInput (const <<< mkHtmlInput $ c))
                  , padding: 24
                  , className: "!bg-white focus:!outline-none w-full h-full"
                  , language: HTML
                  }
              , editor
                  { value: unConvertedModule (convert htmlInput)
                  , onValueChange: (const $ pure unit)
                  , padding: 24
                  , className: "!bg-slate-50 focus:!outline-none"
                  , language: PureScript
                  }
              ]
          }
  where
  initialInput =
    mkHtmlInput <<< String.trim
      $ """
      <section class="a-class">
        <h1>Some heading</h1>
        <p>A paragraph.</p>
      </section>"""
