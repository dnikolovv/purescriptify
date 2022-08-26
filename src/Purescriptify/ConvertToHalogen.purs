module Purescriptify.ConvertToHalogen (convert, HtmlInput, mkHtmlInput, unHtmlInput, ConvertedModule, unConvertedModule) where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Array.NonEmpty (intercalate)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.String (Pattern(..), joinWith, split, stripPrefix)
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
  let
    nodes =
      Array.filter (\n -> n /= (Right ""))
        <<< map renderNode
        <<< Parser.parse $ html

  let
    moduleHeader = intercalate "\n" $ NonEmptyArray
      [ 
        "module PureScript where\n",
       "import Halogen.HTML as HH"
      -- , "import Halogen.HTML.CSS as HHC"
      -- , "import Halogen.HTML.Core as HC"
      -- , "import Halogen.HTML.Elements as HEL"
      , "import Halogen as H"
      , "import Halogen.HTML.Properties as HP"
      , "import Web.HTML.Common as WC"
      , "import Halogen.Query.Event as HQE\n"
      ]

    moduleFooter = intercalate "\n" $ NonEmptyArray
      [ "\nclass_ ∷ ∀ (a ∷ Row Type) (b ∷ Type). String → HH.IProp ( class ∷ String | a ) b"
      , "class_ n = HP.class_ $ WC.ClassName n\n"
      , "attr_ ∷ ∀ (a ∷ Row Type) (b ∷ Type). String → String → HH.IProp a b"
      , "attr_ k v = HP.attr (H.AttrName k) v\n"
      , "classes_ ∷ ∀ (a ∷ Row Type) (b ∷ Type). Array String → HH.IProp ( class ∷ String | a ) b"
      , "classes_ n = HP.classes $ WC.ClassName <$> n\n"
      ]

  let
    headerBody = Array.foldl
      ( \moduleSoFar (nodeIx /\ convertedNode) ->
          case convertedNode of
            Right converted ->
              if converted /= "" then do
                let nodeName = "node" <> show nodeIx
                moduleSoFar <> "\n" <> nodeName <> " = " <> converted
              else moduleSoFar
            Left err ->
              moduleSoFar <> "\n\n-- " <> show err <> "\n"
      )
      moduleHeader
      (Array.mapWithIndex (\ix n -> (ix /\ n)) nodes)
    assembledModule = headerBody <> moduleFooter

  case parseModule assembledModule of
    ParseSucceeded parsedModule ->
      ConvertedModule
        <<< (\x -> maybe x identity (stripPrefix (Pattern "module PureScript where\n\n") x))
        <<< printModule
        <<< toDoc
        <<< formatModule defaultFormatOptions $ parsedModule
    -- We are going to pretend that this is impossible
    -- as we have control over the module structure
    _ -> ConvertedModule assembledModule
  where
  printModule = Dodo.print Dodo.plainText
    { pageWidth: 80
    , ribbonRatio: 1.0
    , indentWidth: 2
    , indentUnit: power " " 2
    }

renderNode :: HtmlNode -> Either ConversionError String
renderNode = case _ of
  HtmlElement { name, attributes, children } ->
    if Array.elem name unsupportedElements then Left $ UnsupportedElement name
    else do
      let
        psAttributes = Array.intercalate ", " <<< map renderAttribute $ attributes
        psChildren =
          case Array.uncons children of
            Nothing -> ""
            Just _ -> Array.intercalate ", "
              ( Array.filter (not <<< (==) "")
                  <<< rights
                  <<< map renderNode $ children
              )
        pref = name <>
          if length attributes == 0 then "_"
          else " [ " <> psAttributes <> " ] "
      pure $ "HH." <> pref <> " [ " <> psChildren <> " ]"
  HtmlText text ->
    let
      contents = String.trim text
    in
      Right $
        if contents /= "" then "HH.text " <> "\"" <> contents <> "\""
        else ""
  HtmlComment _ -> Right ""
  where
  renderAttribute :: HtmlAttribute -> String
  renderAttribute (HtmlAttribute key value) =
    let
      classes = split (Pattern " ") value
      quoted = map (\x -> "\"" <> x <> "\"") classes
    in
      toPureScriptAttributeName key <> " [" <> joinWith ", " quoted <> "]"

  -- TODO: Handle data and aria
  toPureScriptAttributeName :: String -> String
  toPureScriptAttributeName attr = case toCamelCase attr of
    "class" -> "classes_"
    other -> other

  unsupportedElements = [ "svg", "script", "path" ]

rights :: forall a b. Array (Either a b) -> Array b
rights = Array.mapMaybe $ case _ of
  Right a -> Just a
  Left _ -> Nothing