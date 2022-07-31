module Purescriptify.Root where

import Prelude

import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Uncurried (runEffectFn1)
import Foreign.Toast (toastContainer)
import Purescriptify.Capability.Global (class MonadGlobal)
import Purescriptify.Capability.Halo (class MonadHalo, component)
import Purescriptify.Capability.Log (class LogMessages, logDebug, logInfo)
import Purescriptify.Capability.Now (class Now)
import Purescriptify.Capability.Routing (class MonadRouting, navigate)
import Purescriptify.Capability.Routing as Routing
import Purescriptify.Capability.Users (class MonadUsers)
import Purescriptify.Component.GlobalContext (mkGlobalContext)
import Purescriptify.Config as Config
import Purescriptify.Data.Route (Route(..))
import Purescriptify.Page.Home (mkHomePage)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import React.Halo as Halo

data Action
  = Initialize
  | UpdateRoute Route
  | Navigate Route

type Props
  = {}

mkRoot ::
  forall m.
  MonadAff m =>
  MonadGlobal m =>
  MonadRouting m =>
  MonadHalo m =>
  MonadUsers m =>
  Now m =>
  LogMessages m =>
  m (Props -> React.JSX)
mkRoot = do
  render <- mkRender
  component "Root" { context, initialState, eval, render }
  where
  context _ = pure unit

  initialState _ _ =
    { route: Error
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      -- routing
      handleAction <<< UpdateRoute =<< Routing.read
      Routing.subscribe UpdateRoute
    UpdateRoute route -> do
      modify_ _ { route = route }
    Navigate route -> do
      Routing.navigate route

  mkRender = do
    homePage <- mkHomePage
    globalContext <- mkGlobalContext
    pure
      $ \{ state } ->
          let
            contents = case state.route of
              Home -> homePage {}
              Error -> homePage {}
          in
            React.fragment
              [ globalContext unit
              , toastContainer
              , contents
              ]
