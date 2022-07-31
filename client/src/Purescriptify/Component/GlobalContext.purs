module Purescriptify.Component.GlobalContext where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Foreign.Toast (toast, toastError)
import Purescriptify.API.Error (APIErrorContents(..), withErrorContents)
import Purescriptify.API.Types (Error(..))
import Purescriptify.Capability.Global (class MonadGlobal, GlobalAction, GlobalEvent(..), subscribeForGlobalActions, subscribeForGlobalEvents)
import Purescriptify.Capability.Halo (class MonadHalo, component)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import React.Halo as Halo

data Action
  = Initialize
  | HandleGlobalAction GlobalAction
  | HandleGlobalEvent GlobalEvent

mkGlobalContext ::
  forall m.
  MonadGlobal m =>
  MonadHalo m =>
  MonadEffect m =>
  m (Unit -> React.JSX)
mkGlobalContext = component "GlobalContext" { context, initialState, eval, render }
  where
  context _ = pure unit

  initialState _ _ = unit

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction a = case a of
    Initialize -> do
      subscribeForGlobalActions HandleGlobalAction
      subscribeForGlobalEvents HandleGlobalEvent
    HandleGlobalAction globalAction -> case globalAction of
      _ -> pure unit
    HandleGlobalEvent ev -> case ev of
      APIErrorOccurred apiError -> do
        withErrorContents apiError
          $ \err -> case err of
              ValidationError (Error { error }) -> toastError error
              UnknownError _ -> toastError "An unknown error occurred!"
        pure unit

  render _ = R.div {}
