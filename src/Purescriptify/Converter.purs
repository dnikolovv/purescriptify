module Purescriptify.Converter (convert, HtmlInput, mkHtmlInput, unHtmlInput, ConvertedModule, unConvertedModule) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String as String
import Data.String.Casing (toCamelCase)
import Data.Tuple.Nested ((/\))
import Dodo as Dodo
import PureScript.CST (RecoveredParserResult(..), parseModule)
import Purescriptify.HTML.Parser (HtmlAttribute(..), HtmlNode(..))
import Purescriptify.HTML.Parser as Parser
import Safe.Coerce (coerce)
import Tidy (defaultFormatOptions, formatModule, toDoc)

newtype HtmlInput = HtmlInput String

unHtmlInput :: HtmlInput -> String
unHtmlInput = coerce

mkHtmlInput :: String -> HtmlInput
mkHtmlInput = HtmlInput

newtype ConvertedModule = ConvertedModule String

unConvertedModule :: ConvertedModule -> String
unConvertedModule = coerce

data ConversionError =
  UnsupportedElement String

derive instance Eq ConversionError

instance Show ConversionError where
  show (UnsupportedElement elem) =
    "Element '" <> elem <> "' is not supported."

convert :: HtmlInput -> ConvertedModule
convert (HtmlInput html) = do
  let nodes =
        Array.filter (\n -> n /= (Right ""))
          <<< map renderNode
          <<< Parser.parse $ html
  
  let moduleHeader = "module PureScript where \n\nimport React.Basic.DOM as R \n"

  let assembledModule = Array.foldl (\moduleSoFar (nodeIx /\ convertedNode) ->
        case convertedNode of
          Right converted ->
            if converted /= ""
            then do
              let nodeName = "node" <> show nodeIx
              moduleSoFar <> "\n" <> nodeName <> " = " <> converted
            else moduleSoFar
          Left err ->
            moduleSoFar <> "\n\n-- " <> show err <> "\n") moduleHeader (Array.mapWithIndex (\ix n -> (ix /\ n)) nodes)
  
  case parseModule assembledModule of
    ParseSucceeded parsedModule ->
      ConvertedModule
        <<< printModule
        <<< toDoc
        <<< formatModule defaultFormatOptions $ parsedModule
    -- We are going to pretend that this is impossible
    -- as we have control over the module structure
    _ -> ConvertedModule ""
  where
    printModule = Dodo.print Dodo.plainText
      { pageWidth: 80
      , ribbonRatio: 1.0
      , indentWidth: 2
      , indentUnit: power " " 2
      }

renderNode :: HtmlNode -> Either ConversionError String
renderNode = case _ of
  HtmlElement {name, attributes, children} ->
    if Array.elem name unsupportedElements
      then Left $ UnsupportedElement name
      else do
        let psAttributes =
              let result = Array.intercalate ", " <<< map renderAttribute $ attributes
               in if result == ""
                then result
                else result <> ", "
            psChildren =
              case Array.uncons children of
                Nothing -> "children: []"
                Just _ ->
                  "children: ["
                    <> Array.intercalate ", " (Array.filter (not <<< (==) "") <<< rights <<< map renderNode $ children)
                    <> "]"
        pure $ "R." <> name <> "{" <> psAttributes <> psChildren <> "}"
  HtmlText text ->
    let contents = String.trim text
     in Right $ if contents /= ""
      then "R.text" <> "\"" <> contents <> "\""
      else ""
  HtmlComment _ -> Right ""
  where
    renderAttribute :: HtmlAttribute -> String
    renderAttribute (HtmlAttribute key value) =
      toPureScriptAttributeName key <> ": " <> "\"" <> value <> "\""  

    -- TODO: Handle data and aria
    toPureScriptAttributeName :: String -> String
    toPureScriptAttributeName attr = case toCamelCase attr of
      "class" -> "className"
      other -> other

    unsupportedElements = ["svg"]

rights :: forall a b. Array (Either a b) -> Array b
rights = Array.mapMaybe $ case _ of
  Right a -> Just a
  Left _ -> Nothing