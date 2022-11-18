module LitX.Language
    ( Language(..)
    , readLanguage
    , showLanguage
    , showAllLanguages
    , defaultLanguage
    ) where

import LitX.Prelude

import Data.Aeson
import Data.List (intercalate)

data Language = Bash
    deriving stock (Eq, Ord, Enum, Bounded)

instance ToJSON Language where
    toJSON = toJSON . showLanguage
    toEncoding = toEncoding . showLanguage

readLanguage :: String -> Either String Language
readLanguage = \case
    "bash" -> Right Bash
    x ->
        Left
            $ "Invalid language ("
            <> show x
            <> "), must be "
            <> showAllLanguages

showLanguage :: Language -> String
showLanguage = \case
    Bash -> "bash"

showAllLanguages :: String
showAllLanguages = intercalate "|" $ map showLanguage [minBound .. maxBound]

defaultLanguage :: Language
defaultLanguage = Bash
