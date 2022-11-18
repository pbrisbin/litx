module LitX.Parse
    ( Input(..)
    , showInput
    , Markdown
    , markdownPragma
    , markdownCodeBlocks
    , markdownDefaultLanguage
    , parseMarkdown
    ) where

import LitX.Prelude

import CMark
import Data.Aeson
import LitX.CodeBlock
import LitX.Language
import LitX.Options.Pragma

data Input
    = InputStdin
    | InputFile FilePath

instance IsString Input where
    fromString = \case
        "-" -> InputStdin
        path -> InputFile path

instance ToJSON Input where
    toJSON = toJSON . showInput
    toEncoding = toEncoding . showInput

showInput :: Input -> String
showInput = \case
    InputStdin -> "-"
    InputFile path -> path

inputGetContents :: MonadIO m => Input -> m Text
inputGetContents = \case
    InputStdin -> getContents
    InputFile path -> readFile path

data Markdown = Markdown
    { mPragma :: Maybe [String]
    , mCodeBlocks :: [CodeBlock]
    }
    deriving stock Generic
    deriving anyclass ToJSON

markdownPragma :: Markdown -> Maybe [String]
markdownPragma = mPragma

markdownCodeBlocks :: Markdown -> [CodeBlock]
markdownCodeBlocks = mCodeBlocks

markdownDefaultLanguage :: Markdown -> Language
markdownDefaultLanguage =
    fromMaybe defaultLanguage . mostFrequentBy codeBlockLanguage . mCodeBlocks

parseMarkdown :: MonadIO m => Input -> m Markdown
parseMarkdown input = do
    node <- commonmarkToNode [] <$> inputGetContents input

    let mPragma = getPragmaArgs node
        mCodeBlocks = getCodeBlocks src node

    pure Markdown { .. }
    where src = showInput input

getCodeBlocks :: FilePath -> Node -> [CodeBlock]
getCodeBlocks path = walkNodes $ nodeToCodeBlock path

nodeToCodeBlock :: FilePath -> Node -> Maybe CodeBlock
nodeToCodeBlock path = \case
    Node mPosInfo (CODE_BLOCK info content) _ -> do
        language <- hush $ readLanguage $ unpack info
        pure $ codeBlock language path (startLine <$> mPosInfo) content
    _ -> Nothing