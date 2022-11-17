module LitX.CodeBlock
    ( CodeBlock
    , getCodeBlocks
    , renderCodeBlocks
    ) where

import LitX.Prelude

import CMark
import qualified Data.Text as T
import LitX.Language

data CodeBlock = CodeBlock
    { cbLanguage :: Language
    , cbPath :: FilePath
    , cbLine :: Maybe Int
    , cbContent :: Text
    }

getCodeBlocks :: FilePath -> Language -> Node -> [CodeBlock]
getCodeBlocks path language = walkNodes $ nodeToCodeBlock path language

nodeToCodeBlock :: FilePath -> Language -> Node -> Maybe CodeBlock
nodeToCodeBlock path language = \case
    Node mPosInfo (CODE_BLOCK info content) _
        | info `matchesLanguage` language -> Just CodeBlock
            { cbLanguage = language
            , cbPath = path
            , cbLine = startLine <$> mPosInfo
            , cbContent = content
            }
    _ -> Nothing

renderCodeBlocks :: [CodeBlock] -> Text
renderCodeBlocks = T.intercalate "\n" . map renderCodeBlock

renderCodeBlock :: CodeBlock -> Text
renderCodeBlock CodeBlock {..} = maybe "" sourceComment cbLine <> cbContent
  where
    sourceComment l =
        commentInLanguage cbLanguage
            $ "source="
            <> pack cbPath
            <> ":"
            <> pack (show l)
            <> "\n"
