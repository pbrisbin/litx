module LitX.CodeBlock
    ( CodeBlock
    , codeBlockLanguage
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

codeBlockLanguage :: CodeBlock -> Language
codeBlockLanguage = cbLanguage

getCodeBlocks :: FilePath -> Node -> [CodeBlock]
getCodeBlocks path = walkNodes $ nodeToCodeBlock path

nodeToCodeBlock :: FilePath -> Node -> Maybe CodeBlock
nodeToCodeBlock path = \case
    Node mPosInfo (CODE_BLOCK info content) _ -> do
        language <- hush $ readLanguage $ unpack info
        pure CodeBlock
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
