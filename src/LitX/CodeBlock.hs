module LitX.CodeBlock
    ( CodeBlock
    , codeBlock
    , codeBlockIndex
    , codeBlockTag
    , codeBlockPath
    , codeBlockLine
    , codeBlockContent
    ) where

import LitX.Prelude

data CodeBlock = CodeBlock
    { cbIndex :: Natural
    , cbTag :: Text
    , cbPath :: FilePath
    , cbLine :: Maybe Int
    , cbContent :: Text
    }

codeBlock :: Natural -> Text -> FilePath -> Maybe Int -> Text -> CodeBlock
codeBlock = CodeBlock

codeBlockIndex :: CodeBlock -> Natural
codeBlockIndex = cbIndex

codeBlockTag :: CodeBlock -> Text
codeBlockTag = cbTag

codeBlockPath :: CodeBlock -> FilePath
codeBlockPath = cbPath

codeBlockLine :: CodeBlock -> Maybe Int
codeBlockLine = cbLine

codeBlockContent :: CodeBlock -> Text
codeBlockContent = cbContent
