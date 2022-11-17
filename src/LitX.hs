{-# LANGUAGE TupleSections #-}

module LitX
    ( litx
    ) where

import LitX.Prelude

import CMark
import qualified Data.Monoid as Monoid
import LitX.CodeBlock
import LitX.Execute
import LitX.Language
import LitX.Options
import LitX.Options.Pragma
import UnliftIO.Exception (Exception, throwIO)

data LanguageUnknown = LanguageUnknown
    deriving stock Show
    deriving anyclass Exception

litx :: MonadUnliftIO m => [String] -> m ()
litx args = do
    options <- parseOptions args

    (path, markdown) <- case getLast $ oInput options of
        InputStdin -> ("<stdin>", ) <$> getContents
        InputFile p -> (p, ) <$> readFile p

    let node = commonmarkToNode [] markdown
        blocks = getCodeBlocks path node

    Options {..} <- addPragmaOptions options node

    language <-
        maybe (throwIO LanguageUnknown) pure
        $ Monoid.getLast oLanguage
        <|> mostFrequentBy codeBlockLanguage blocks

    let
        executeOptions =
            appEndoDual oModExecuteOptions $ languageExecuteOptions language

    executeScript executeOptions $ renderCodeBlocks $ filter
        ((== language) . codeBlockLanguage)
        blocks

addPragmaOptions :: MonadIO m => Options -> Node -> m Options
addPragmaOptions options =
    maybe (pure options) (fmap (options <>) . parseOptions) . getPragmaArgs
