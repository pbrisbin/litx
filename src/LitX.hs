{-# LANGUAGE TupleSections #-}

module LitX
    ( litx
    ) where

import LitX.Prelude

import CMark
import LitX.CodeBlock
import LitX.Execute
import LitX.Language
import LitX.Options
import LitX.Options.Pragma

litx :: MonadUnliftIO m => [String] -> m ()
litx args = do
    options <- parseOptions args

    (path, markdown) <- case getLast $ oInput options of
        InputStdin -> ("<stdin>", ) <$> getContents
        InputFile p -> (p, ) <$> readFile p

    let node = commonmarkToNode [] markdown

    Options {..} <- addPragmaOptions options node

    let language = getLast oLanguage
        executeOptions =
            appEndo oModExecuteOptions $ languageExecuteOptions language

    executeScript executeOptions $ renderCodeBlocks $ getCodeBlocks
        path
        language
        node

addPragmaOptions :: MonadIO m => Options -> Node -> m Options
addPragmaOptions options =
    maybe (pure options) (fmap (options <>) . parseOptions) . getPragmaArgs
