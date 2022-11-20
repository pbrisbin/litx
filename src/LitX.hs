module LitX
    ( litx
    ) where

import LitX.Prelude

import Blammo.Logging.Simple
import LitX.CodeBlock
import LitX.Execute
import LitX.Language
import LitX.Options
import LitX.Parse

litx :: MonadUnliftIO m => [String] -> m ()
litx args = runSimpleLoggingT $ do
    options <- parseOptions args
    logDebug $ "Options parsed" :# ["input" .= optionsInput options]

    markdown <- parseMarkdown $ optionsInput options
    logDebug $ "Markdown parsed" :# ["markdown" .= markdown]

    mPragmaOptions <- traverse parseExecuteOptions $ markdownPragma markdown

    let
        executeOptions = getExecuteOptions $ mconcat
            [ languageExecuteOptions $ inferredLanguage markdown
            , fromMaybe mempty mPragmaOptions
            , optionsExecuteOptions options
            ]

    logDebug $ "Executing" :# ["executeOptions" .= executeOptions]

    executeMarkdown executeOptions markdown

inferredLanguage :: Markdown -> Language
inferredLanguage =
    fromMaybe defaultLanguage
        . mostFrequent
        . mapMaybe (hush . readLanguage . unpack . codeBlockTag)
        . markdownCodeBlocks
