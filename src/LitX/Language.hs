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

import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Version
import LitX.Execute.Options
import Options.Applicative
import qualified Paths_litx as Pkg

data Language
    = Bash
    | Python
    deriving stock (Eq, Ord, Enum, Bounded)

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
            . (shebangL .~ "/usr/bin/env bash")
            . (bannerL ?~ ("#\n# " <> generatedBy <> "\n#\n###"))
            . (preambleL ?~ "set -euo pipefail")
            . (commentCharsL .~ "#")
            . (execL .~ "bash")
            . (argsL .~ ["-s", "-"])
    Python ->
        (filterL .~ filterLanguage Python)
            . (shebangL .~ "/usr/bin/env python")
            . (bannerL ?~ ("#\n# " <> generatedBy <> "\n#\n###"))
            . (commentCharsL .~ "#")
            . (execL .~ "python")
            . (argsL .~ ["-s", "-"])

languageOptionParser :: Language -> Parser (Dual (Endo ExecuteOptions))
languageOptionParser language =
    flag mempty (languageExecuteOptions language) $ mconcat
        [ long shown
        , help $ "Parse and execute " <> shownTag <> " code-blocks" <> suffix
        ]
  where
    shownTag = unpack $ languageCodeBlockTag language
    shown = showLanguage language
    suffix
        | shown == shownTag = ""
        | otherwise = " as " <> shown

filterLanguage :: Language -> Filter
filterLanguage = filterCodeBlockTag . languageCodeBlockTag

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

generatedBy :: Text
generatedBy = "Generated by litx-v" <> pack (showVersion Pkg.version) <> "."
