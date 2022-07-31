{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Purescriptify.Converter (htmlToPureScript, runFormatWithPursTidy) where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Process (Process, readProcess)
import Purescriptify.Effects.FormatPureScript (FormatPureScript (FormatPureScript), formatPureScript)
import Purescriptify.Types
import Purescriptify.Types.NotEmptyText (unNotEmptyText)
import Purescriptify.Utilities (replace)
import RIO
import qualified RIO.Text as T
import Text.Casing
import Text.XML.Light
import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Char8 as BS

htmlToPureScript ::
  FormatPureScript :> es =>
  Error ConversionError :> es =>
  HtmlInput ->
  Eff es FormattedPureScript
htmlToPureScript input =
  generateFromHtml input
    >>= formatPureScript

runFormatWithPursTidy ::
  ( Process :> es,
    IOE :> es,
    Error ConversionError :> es
  ) =>
  Eff (FormatPureScript : es) a ->
  Eff es a
runFormatWithPursTidy = interpret $ \_ -> \case
  FormatPureScript (GeneratedPureScript ps) -> do
    let file = replace "{code}" (T.unpack ps) fileSkeleton
    FormattedPureScript . T.pack
      <$> readProcess "purs-tidy" ["format", "-w", "80"] file
  where
    fileSkeleton :: String
    fileSkeleton = BS.unpack $(embedFile "format.purs")

generateFromHtml ::
  Error ConversionError :> es =>
  HtmlInput ->
  Eff es GeneratedPureScript
generateFromHtml (HtmlInput html) = case parseXMLDoc (encodeUtf8 . unNotEmptyText $ html) of
  Nothing -> throwError CouldNotParseRootElement
  Just rootElement -> do
    case renderContent (Elem rootElement) of
      Left err -> throwError err
      Right generated -> pure (GeneratedPureScript generated)

renderContent :: Content -> Either ConversionError Text
renderContent = \case
  Elem el -> do
    let name = T.pack . qName . elName $ el

    if name == "svg"
      then Left $ UnsupportedElement "svg"
      else do
        let attributes =
              case T.intercalate ", " $ map renderAttr (elAttribs el) of
                "" -> ""
                something -> something <> ", "
            children =
              case map renderContent (elContent el) of
                [] -> "children: []"
                childEls ->
                  "children: ["
                    <> T.intercalate ", " (filter (not . (==) mempty) . rights $ childEls)
                    <> "]"
        pure $ "R." <> name <> "{" <> attributes <> "" <> children <> "}"
  Text cd ->
    pure $
      let contents = T.strip . T.pack . cdData $ cd
       in if contents /= mempty
            then "R.text " <> "\"" <> contents <> "\""
            else mempty
  CRef _ -> pure mempty
  where
    renderAttr :: Attr -> Text
    renderAttr (Attr key val) =
      toPureScriptAttributeName key <> ": " <> "\"" <> T.pack val <> "\""

    -- TODO: handle data and aria
    toPureScriptAttributeName :: QName -> Text
    toPureScriptAttributeName key =
      case toSnake . fromAny . qName $ key of
        "class" -> "className"
        other -> T.pack other