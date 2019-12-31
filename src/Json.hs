module Json where

import ClassyPrelude hiding (toLower)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (toLower)

genericParseWithPrefix prefix = genericParseJSON $ defaultOptions {fieldLabelModifier = fieldLabelNoPrefix prefix}

genericToJSONNoPrefix prefix = genericToJSON $ defaultOptions {fieldLabelModifier = fieldLabelNoPrefix prefix}

fieldLabelNoPrefix :: String -> String -> String
fieldLabelNoPrefix prefix = firstLetterToLowercase . drop (length (prefix :: String))

firstLetterToLowercase :: String -> String
firstLetterToLowercase [] = []
firstLetterToLowercase (l:remainder) = toLower l : remainder