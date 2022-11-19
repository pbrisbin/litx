module LitX.CodeBlock
    ( CodeBlock
    , codeBlock
    , codeBlockLanguage
    , codeBlockPath
    , codeBlockLine
    , codeBlockContent
    , codeBlockIsLanguage
    ) where

import LitX.Prelude

import Data.Aeson
import Data.Function (on)
import LitX.Language

data CodeBlock = CodeBlock
    { cbLanguage :: Language
    , cbPath :: FilePath
    , cbLine :: Maybe Int
    , cbContent :: Text
    }
    deriving stock Generic
    deriving anyclass ToJSON

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

codeBlockIsLanguage :: Language -> CodeBlock -> Bool
codeBlockIsLanguage language = go language . codeBlockLanguage
    where go = (==) `on` languageCodeBlockTag
