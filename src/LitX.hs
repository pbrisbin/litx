module LitX
    ( litx
    ) where

import LitX.Prelude

import LitX.Execute
import LitX.Language
import LitX.Options
import LitX.Parse
import LitX.Parse.Markdown

litx :: MonadUnliftIO m => [String] -> m ()
litx args = do
    options <- parseOptions args
    markdown <- parseMarkdown $ optionsInput options

    let
        executeOptions = getExecuteOptions $ mconcat
            [ languageExecuteOptions $ markdownInferredLanguage markdown
            , markdownPragma markdown
            , optionsExecuteOptions options
            ]

    executeMarkdown executeOptions markdown
