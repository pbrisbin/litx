module LitX.CodeBlock
    ( CodeBlock
    , codeBlock
    , codeBlockLanguage
    , codeBlockPath
    , codeBlockLine
    , codeBlockContent
    ) where

import LitX.Prelude

import LitX.Language

data CodeBlock = CodeBlock
    { cbLanguage :: Language
    , cbPath :: FilePath
    , cbLine :: Maybe Int
    , cbContent :: Text
    }

codeBlock :: Language -> FilePath -> Maybe Int -> Text -> CodeBlock
codeBlock = CodeBlock

codeBlockLanguage :: CodeBlock -> Language
codeBlockLanguage = cbLanguage

codeBlockPath :: CodeBlock -> FilePath
codeBlockPath = cbPath

codeBlockLine :: CodeBlock -> Maybe Int
codeBlockLine = cbLine

codeBlockContent :: CodeBlock -> Text
codeBlockContent = cbContent
