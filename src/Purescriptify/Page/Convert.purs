module Purescriptify.Page.Convert where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Editor (EditorLanguage(..), editor)
import Purescriptify.Converter (convert, mkHtmlInput, unConvertedModule, unHtmlInput)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.Window (sessionStorage)
import Web.Storage.Storage (getItem, setItem)

mkConverter :: Effect (Unit -> React.JSX)
mkConverter = do
  let sessionInputKey = "purescriptify:input"
  session <- window >>= sessionStorage

  initialInput <- getItem sessionInputKey session >>= case _ of
    Just existing -> pure (mkHtmlInput existing)
    Nothing -> pure defaultInput

  React.component "Converter" \_ -> React.do
    (htmlInput /\ setHtmlInput) <- React.useState initialInput

    let onInputChange new = do
          setHtmlInput (const <<< mkHtmlInput $ new)
          setItem sessionInputKey new session

    pure
      $ R.section
          { className: "w-full h-screen grid grid-cols-2"
          , children:
              [ editor
                  { value: unHtmlInput htmlInput
                  , onValueChange: onInputChange
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
  defaultInput =
    mkHtmlInput <<< String.trim
      $ """
<section class="a-class">
  <h1>Some heading</h1>
  <p>A paragraph.</p>
</section>"""
