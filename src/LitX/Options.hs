module LitX.Options
    ( Options(..)
    , Input(..)
    , Shebang
    , renderShebang
    , Preamble(..)
    , CodeBlockTag(..)
    , ExecutionMode(..)
    , ExecuteProcessArg(..)
    , Output(..)
    , parseOptions
    , addPragmaOptions
    ) where

import LitX.Prelude

import CMark

data Options = Options
    { oInput :: Input
    , oShebang :: Shebang
    , oPreamble :: Preamble
    , oCodeBlockTag :: CodeBlockTag
    , oExecutionMode :: ExecutionMode
    }

data Input
    = InputStdin
    | InputFile FilePath

newtype Shebang = Shebang
    { unShebang :: Text
    }

renderShebang :: Shebang -> Text
renderShebang = ("#!" <>) . unShebang

newtype Preamble = Preamble
    { unPreamble :: Text
    }

data CodeBlockTag = Bash

data ExecutionMode
    = ExecuteStdin String [String]
    | ExecuteProcess String [ExecuteProcessArg]
    | NoExecute Output

data ExecuteProcessArg
    = ExecuteProcessArgPlain
    | ExecuteProcessArgScript

data Output
    = OutputNone
    | OutputStdout
    | OutputFile FilePath

parseOptions :: MonadIO m => [String] -> m Options
parseOptions _ = pure Options
    { oInput = InputFile "TODO"
    , oShebang = Shebang "/usr/bin/env bash"
    , oPreamble = Preamble "set -euo pipefail"
    , oCodeBlockTag = Bash
    , oExecutionMode = ExecuteStdin "bash" []
    }

addPragmaOptions :: Node -> Options -> m Options
addPragmaOptions = undefined
