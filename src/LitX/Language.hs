module LitX.Language
    ( Language(..)
    , langOption
    , languageExecuteOptions
    , commentInLanguage
    , matchesLanguage
    ) where

import LitX.Prelude

import CMark (Info)
import Data.List (intercalate)
import Data.Version
import LitX.Execute
import Options.Applicative
    ( Mod
    , OptionFields
    , Parser
    , eitherReader
    , metavar
    , option
    , showDefaultWith
    , value
    )
import qualified Paths_litx as Pkg

data Language = Bash
    deriving stock (Enum, Bounded)

defaultLanguage :: Language
defaultLanguage = Bash

readLanguage :: String -> Either String Language
readLanguage = \case
    "bash" -> Right Bash
    x -> Left $ "Invalid language (" <> show x <> "), must be " <> languages

showLanguage :: Language -> String
showLanguage = \case
    Bash -> "bash"

languages :: String
languages = intercalate "|" $ map showLanguage [minBound .. maxBound]

langOption :: Mod OptionFields Language -> Parser Language
langOption m = option
    (eitherReader readLanguage)
    (m
    <> metavar languages
    <> value defaultLanguage
    <> showDefaultWith showLanguage
    )

languageExecuteOptions :: Language -> ExecuteOptions
languageExecuteOptions = \case
    Bash -> ExecuteOptions
        { eoShebang = "/usr/bin/env bash\n"
        , eoBanner = "#\n# " <> generatedBy <> "\n#\n###\n"
        , eoPreamble = "set -euo pipefail"
        , eoExecuteMode = Execute "bash"
        , eoExecuteArgs = ["-s", "-"]
        , eoInheritEnv = InheritEnv
        , eoOutput = OutputFile "/dev/null"
        }

generatedBy :: Text
generatedBy = pack $ "Generated by litx-v" <> showVersion Pkg.version <> "."

commentInLanguage :: Language -> Text -> Text
commentInLanguage = \case
    Bash -> ("# " <>)

matchesLanguage :: Info -> Language -> Bool
matchesLanguage info = \case
    Bash -> info == "bash"
