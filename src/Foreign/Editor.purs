module Foreign.Editor (editor, Code, EditorProps, EditorLanguage (..)) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)
import Foreign (Foreign)
import React.Basic (ReactComponent)
import React.Basic.DOM (CSS, css)
import React.Basic.Hooks (JSX, element)

type Code
  = String

type EditorProps
  = { value :: Code
    , onValueChange :: Code -> Effect Unit
    , padding :: Int
    , className :: String
    , language :: EditorLanguage
    }

data EditorLanguage = HTML | PureScript

editor :: EditorProps -> JSX
editor { value, onValueChange, padding, language, className } =
  element editor_
    { value
    , onValueChange: mkEffectFn1 onValueChange
    , highlight: mkEffectFn1 (\code ->
        let lang = case language of
              PureScript -> languages_.purescript
              HTML -> languages_.markup
        in runEffectFn2 highlight_ code lang)
    , padding
    , textareaClassName: className
    , style: css {}
    }

type RawEditorProps
  = { value :: Code
    , onValueChange :: EffectFn1 Code Unit
    , highlight :: EffectFn1 Code Unit
    , padding :: Int
    , textareaClassName :: String
    , style :: CSS
    }

type Language = Foreign

type Languages = {
  markup :: Language,
  purescript :: Language
}

foreign import editor_ :: ReactComponent RawEditorProps

foreign import languages_ :: Languages

foreign import highlight_ :: EffectFn2 Code Language Unit