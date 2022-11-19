module LitX.CodeBlock
    ( CodeBlock
    , codeBlock
    , codeBlockTag
    , codeBlockPath
    , codeBlockLine
    , codeBlockContent
    ) where

import LitX.Prelude

import Data.Aeson

data CodeBlock = CodeBlock
    { cbTag :: Text
    , cbPath :: FilePath
    , cbLine :: Maybe Int
    , cbContent :: Text
    }
    deriving stock Generic
    deriving anyclass ToJSON

codeBlock :: Text -> FilePath -> Maybe Int -> Text -> CodeBlock
codeBlock = CodeBlock

codeBlockTag :: CodeBlock -> Text
codeBlockTag = cbTag

codeBlockPath :: CodeBlock -> FilePath
codeBlockPath = cbPath

codeBlockLine :: CodeBlock -> Maybe Int
codeBlockLine = cbLine

codeBlockContent :: CodeBlock -> Text
codeBlockContent = cbContent
