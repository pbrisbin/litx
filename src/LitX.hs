module LitX
    ( litx
    ) where

import LitX.Prelude

import Blammo.Logging.Simple
import LitX.Execute
import LitX.Options
import LitX.Parse

litx :: MonadUnliftIO m => [String] -> m ()
litx args = runSimpleLoggingT $ do
    options <- parseOptions args
    logDebug $ "Options parsed" :# ["input" .= optionsInput options]

    markdown <- parseMarkdown $ optionsInput options
    logDebug $ "Markdown parsed" :# ["markdown" .= markdown]

    executeOptions <-
        (`getExecuteOptions` markdownDefaultLanguage markdown) <$> maybe
            (pure $ optionsExecuteOptions options)
            (fmap (<> optionsExecuteOptions options) . parseExecuteOptions)
            (markdownPragma markdown)

    logDebug $ "Executing" :# ["options" .= executeOptions]
    executeMarkdown executeOptions markdown
