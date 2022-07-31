{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Purescriptify.Effects.FormatPureScript where

import Effectful
import Effectful.TH
import Purescriptify.Types (FormattedPureScript, GeneratedPureScript)

data FormatPureScript :: Effect where
  FormatPureScript :: GeneratedPureScript -> FormatPureScript m FormattedPureScript

type instance DispatchOf FormatPureScript = 'Dynamic

makeEffect ''FormatPureScript