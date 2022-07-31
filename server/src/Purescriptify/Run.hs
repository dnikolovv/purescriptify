{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Purescriptify.Run where

import Control.Monad.Except (ExceptT (ExceptT))
import qualified Data.Aeson as JSON
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Process (Process, runProcess)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Errors (errorMwDefJson)
import Purescriptify.API.Definition (API, server)
import qualified Purescriptify.API.Definition as API
import Purescriptify.Converter (runFormatWithPursTidy)
import Purescriptify.Effects.FormatPureScript (FormatPureScript)
import Purescriptify.Types (ConversionError (..))
import RIO
import qualified Servant
import Servant.Server (ServerError (errBody), err400, hoistServer, serve)
import System.IO (putStrLn)

run :: IO ()
run =
  runSettings
    ( setPort port $
        setBeforeMainLoop
          (putStrLn $ "Running on port " <> show port)
          defaultSettings
    )
    . corsMiddleware allowedCors
    . errorMwDefJson
    . logStdoutDev
    $ waiApp
  where
    waiApp =
      serve api (hoistServer api effToHandler server)
    port = 3005
    allowedCors = (["http://localhost:1234"], True)
    api = Proxy @API

effToHandler :: Eff [FormatPureScript, Process, Error ConversionError, IOE] a -> Servant.Handler a
effToHandler m = do
  result <- liftIO . runEff . runErrorNoCallStack @ConversionError . runProcess . runFormatWithPursTidy $ m

  Servant.Handler $ ExceptT (pure . mapLeft fromConversionError $ result)
  where
    fromConversionError e =
      err400
        { errBody = JSON.encode $ API.KnownError e
        }

type SendCredentials = Bool

type Origins = ([Origin], SendCredentials)

corsMiddleware :: Origins -> Middleware
corsMiddleware origins =
  cors $
    const $
      Just $
        simpleCorsResourcePolicy
          { corsMethods = ["PUT", "GET", "DELETE", "HEAD", "OPTIONS", "POST"],
            corsRequestHeaders = ["content-type", "authorization", "sentry-trace"],
            corsOrigins = Just origins
          }