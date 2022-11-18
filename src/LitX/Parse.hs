module LitX.Parse
    ( ParseOptions
    , defaultParseOptions
    , Input(..)
    , parseCodeBlocks
    ) where

import LitX.Prelude

import CMark
import LitX.CodeBlock

data ParseOptions = ParseOptions

defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions

data Input
    = InputStdin
    | InputFile FilePath

instance IsString Input where
    fromString = \case
        "-" -> InputStdin
        path -> InputFile path

showInput :: Input -> String
showInput = \case
    InputStdin -> "-"
    InputFile path -> path

inputGetContents :: MonadIO m => Input -> m Text
inputGetContents = \case
    InputStdin -> getContents
    InputFile path -> readFile path

parseCodeBlocks :: MonadIO m => ParseOptions -> Input -> m [CodeBlock]
parseCodeBlocks _ input = do
    getCodeBlocks src . commonmarkToNode [] <$> inputGetContents input
    where src = showInput input

getCodeBlocks :: FilePath -> Node -> [CodeBlock]
getCodeBlocks path = walkNodes $ nodeToCodeBlock path

nodeToCodeBlock :: FilePath -> Node -> Maybe CodeBlock
nodeToCodeBlock path = \case
    Node mPosInfo (CODE_BLOCK info content) _ -> do
        language <- hush $ readLanguage $ unpack info
        pure $ codeBlock language path (startLine <$> mPosInfo) content
    _ -> Nothing
