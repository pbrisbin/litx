module LitX.Parse.Markdown
    ( Markdown(..)
    , MarkdownItem(..)
    , markdownPragma
    , markdownCodeBlocks
    , markdownInferredLanguage
    , markdownSource
    ) where

import LitX.Prelude

import Conduit
import Data.Conduit.List (sourceList)
import LitX.CodeBlock
import LitX.Execute.Options
import LitX.Language

newtype Markdown = Markdown
    { unMarkdown :: [MarkdownItem]
    }
    deriving newtype (Semigroup, Monoid)

data MarkdownItem
    = MarkdownPragma [String] (Dual (Endo ExecuteOptions))
    | MarkdownPragmaError [String] String
    | MarkdownFilterPragma Filter
    | MarkdownCodeBlock CodeBlock

markdownPragma :: Markdown -> Dual (Endo ExecuteOptions)
markdownPragma =
    fromMaybe mempty
        . listToMaybe
        . mapMaybe
              (\case
                  MarkdownPragma _ os -> Just os
                  MarkdownPragmaError{} -> Nothing
                  MarkdownFilterPragma{} -> Nothing
                  MarkdownCodeBlock{} -> Nothing
              )
        . unMarkdown

markdownCodeBlocks :: Markdown -> [CodeBlock]
markdownCodeBlocks =
    mapMaybe
            (\case
                MarkdownPragma{} -> Nothing
                MarkdownPragmaError{} -> Nothing
                MarkdownFilterPragma{} -> Nothing
                MarkdownCodeBlock cb -> Just cb
            )
        . unMarkdown


markdownInferredLanguage :: Markdown -> Language
markdownInferredLanguage =
    fromMaybe defaultLanguage
        . mostFrequent
        . mapMaybe (hush . readLanguage . unpack . codeBlockTag)
        . markdownCodeBlocks

markdownSource :: Monad m => Markdown -> ConduitT () MarkdownItem m ()
markdownSource = sourceList . unMarkdown
