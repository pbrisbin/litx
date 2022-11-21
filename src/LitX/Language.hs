module LitX.Language
    ( Language(..)
    , languageCodeBlockTag
    , languageOptionParser
    , languageExecuteOptions
    , readLanguage
    , showLanguage
    , showAllLanguages
    , defaultLanguage
    ) where

import LitX.Prelude

import Data.Aeson
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LitX.CodeBlock
import LitX.Execute
import Options.Applicative

data Language
    = Bash
    | Python
    deriving stock (Eq, Ord, Enum, Bounded)

instance ToJSON Language where
    toJSON = toJSON . showLanguage
    toEncoding = toEncoding . showLanguage

showLanguage :: Language -> String
showLanguage = \case
    Bash -> "bash"
    Python -> "python"

languageCodeBlockTag :: Language -> Text
languageCodeBlockTag = \case
    Bash -> "bash"
    Python -> "python"

languageExecuteOptions :: Language -> Dual (Endo ExecuteOptions)
languageExecuteOptions = Dual . Endo . \case
    Bash ->
        (filterL .~ filterLanguage Bash)
            . (commentCharsL .~ "#")
            . (execL .~ "bash")
            . (argsL .~ ["-s", "-"])
    Python ->
        (filterL .~ filterLanguage Python)
            . (execL .~ "python")
            . (argsL .~ ["-s", "-"])

languageOptionParser :: Language -> Parser (Dual (Endo ExecuteOptions))
languageOptionParser language =
    flag mempty (languageExecuteOptions language) $ mconcat
        [ long shown
        , help $ "Parse and execute " <> shownTag <> " code blocks" <> suffix
        ]
  where
    shownTag = unpack $ languageCodeBlockTag language
    shown = showLanguage language
    suffix
        | shown == shownTag = ""
        | otherwise = " as " <> shown

filterLanguage :: Language -> Filter
filterLanguage language =
    Filter $ (== languageCodeBlockTag language) . codeBlockTag

readLanguage :: String -> Either String Language
readLanguage x = note err $ Map.lookup x allLanguages
  where
    err = "Invalid language (" <> show x <> "), must be " <> showAllLanguages

showAllLanguages :: String
showAllLanguages = intercalate "|" $ Map.keys allLanguages

allLanguages :: Map String Language
allLanguages = Map.fromList $ map (showLanguage &&& id) [minBound .. maxBound]

defaultLanguage :: Language
defaultLanguage = Bash
