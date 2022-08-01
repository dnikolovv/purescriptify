module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Purescriptify.Page.Convert (mkConverter)
import React.Basic.DOM (render) as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  rootMay <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case rootMay of
    Just root -> do
      rootComponent <- mkConverter
      R.render (rootComponent unit) root
    Nothing -> log "No root element."
