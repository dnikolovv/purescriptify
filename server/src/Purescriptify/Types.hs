{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Purescriptify.Types where

import Data.Aeson (FromJSON, ToJSON)
import Purescriptify.Types.NotEmptyText (NotEmptyText)
import RIO

data ConversionError
  = CouldNotParseRootElement
  | UnsupportedElement Text
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Exception ConversionError

newtype GeneratedPureScript = GeneratedPureScript Text
  deriving (Generic)
  deriving newtype (FromJSON, ToJSON)

newtype FormattedPureScript = FormattedPureScript Text
  deriving (Generic)
  deriving newtype (FromJSON, ToJSON)

newtype HtmlInput = HtmlInput NotEmptyText
  deriving (Generic)
  deriving newtype (FromJSON, ToJSON)