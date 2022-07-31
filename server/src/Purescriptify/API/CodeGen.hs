{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Purescriptify.API.CodeGen where

import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes
import Purescriptify.API.Definition (API, ConversionRequest, ConversionResponse)
import Purescriptify.Types (ConversionError, FormattedPureScript, HtmlInput)
import RIO
import Servant.PureScript (HasBridge (..), Settings, addTypes, defaultSettings, generateWithSettings)

codegen :: String -> IO ()
codegen destination =
  genPureScriptTypes destination
    >> genServant destination

genServant :: String -> IO ()
genServant dir =
  generateWithSettings
    mySettings
    dir
    myBridgeProxy
    (Proxy @API)

data MyBridge

instance HasBridge MyBridge where
  languageBridge _ = buildBridge bridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

mySettings :: Settings
mySettings =
  defaultSettings
    & addTypes myTypes

genPureScriptTypes :: String -> IO ()
genPureScriptTypes destination =
  writePSTypes
    destination
    (buildBridge bridge)
    myTypes

bridge :: BridgePart
bridge =
  notEmptyTextBridge
    <|> utcTimeBridge
    <|> defaultBridge

-- We'll translate our NotEmptyText to plain text
notEmptyTextBridge :: BridgePart
notEmptyTextBridge = do
  typeName ^== "NotEmptyText"
  pure psString

utcTimeBridge :: BridgePart
utcTimeBridge = do
  typeName ^== "UTCTime"
  pure psUTCTime

psUTCTime :: PSType
psUTCTime = TypeInfo "haskell-iso" "Data.Argonaut.JSONDateTime" "JSONDateTime" []

myTypes :: [SumType 'Haskell]
myTypes =
  [ genericShow $ equal $ argonaut $ mkSumType @ConversionRequest,
    genericShow $ equal $ argonaut $ mkSumType @ConversionResponse,
    genericShow $ equal $ argonaut $ mkSumType @HtmlInput,
    genericShow $ equal $ argonaut $ mkSumType @FormattedPureScript,
    genericShow $ equal $ argonaut $ mkSumType @ConversionError
  ]