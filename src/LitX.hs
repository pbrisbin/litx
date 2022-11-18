module LitX
    ( litx
    ) where

import LitX.Prelude

import LitX.Execute
import LitX.Options
import LitX.Parse

litx :: MonadUnliftIO m => [String] -> m ()
litx args = do
    options <- parseOptions args
    markdown <- parseMarkdown $ optionsInput options

    executeOptions <-
        (`getExecuteOptions` markdownDefaultLanguage markdown) <$> maybe
            (pure $ optionsExecuteOptions options)
            (fmap (<> optionsExecuteOptions options) . parseExecuteOptions)
            (markdownPragma markdown)

    executeMarkdown executeOptions markdown
