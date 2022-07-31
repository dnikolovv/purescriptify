{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Purescriptify.API.Definition where

import Data.Aeson (FromJSON, ToJSON)
import Effectful (Eff)
import qualified Effectful as E
import qualified Effectful.Error.Static as ES
import Purescriptify.Converter (htmlToPureScript)
import Purescriptify.Effects.FormatPureScript (FormatPureScript)
import Purescriptify.Types (ConversionError, FormattedPureScript, HtmlInput)
import RIO
import Servant

type API =
  "convert" :> ReqBody '[JSON] ConversionRequest :> Post '[JSON] ConversionResponse

server ::
  FormatPureScript E.:> es =>
  ES.Error ConversionError E.:> es =>
  ServerT API (Eff es)
server =
  publicServer
  where
    publicServer (ConversionRequest html) =
      ConversionResponse <$> htmlToPureScript html

newtype ConversionRequest = ConversionRequest
  { html :: HtmlInput
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ConversionResponse = ConversionResponse
  { purescript :: FormattedPureScript
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype KnownError = KnownError
  { errType :: ConversionError
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)