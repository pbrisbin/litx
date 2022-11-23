module LitX.Interactive
    ( handleCodeBlocks
    ) where

import LitX.Prelude

import Control.Monad (foldM_)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans (lift)
import Data.Bool (bool)
import qualified Data.Text as T
import LitX.CodeBlock
import System.Console.Haskeline

data InteractiveState
    = Prompting
    | ExecutingRest
    | SkippingRest

data PromptResult
    = Execute
    | ExecuteRest
    | Skip
    | SkipRest
    -- | Edit

handleCodeBlocks
    :: MonadUnliftIO m => [CodeBlock] -> m () -> (CodeBlock -> m ()) -> m ()
handleCodeBlocks blocks checkProcess onExecute = withRunInIO $ \runInIO ->
    runInputT defaultSettings $ foldM_
        (handleCodeBlock (runInIO checkProcess) (runInIO . onExecute))
        Prompting
        blocks

handleCodeBlock
    :: IO ()
    -> (CodeBlock -> IO ())
    -> InteractiveState
    -> CodeBlock
    -> InputT IO InteractiveState
handleCodeBlock checkProcess onExecute executing block = do
    lift checkProcess

    case executing of
        Prompting -> do
            result <- promptCodeBlock block

            case result of
                Execute -> do
                    outputTextLn "Executing... press any key to continue."
                    lift $ onExecute block
                    bool SkippingRest executing <$> waitForAnyKey ""
                ExecuteRest -> ExecutingRest <$ lift (onExecute block)
                Skip -> pure executing
                SkipRest -> pure SkippingRest

        ExecutingRest -> executing <$ lift (onExecute block)
        SkippingRest -> pure SkippingRest

promptCodeBlock :: CodeBlock -> InputT IO PromptResult
promptCodeBlock block = do
    outputTextLn
        $ "  ╔══[ "
        <> codeBlockTag block
        <> "/"
        <> pack (show $ codeBlockIndex block)
        <> ": "
        <> pack (codeBlockPath block)
        <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)
        <> " ]"
    outputTextLn "  ║ "
    traverse_ (outputTextLn . ("  ║ " <>)) $ T.lines $ codeBlockContent block
    outputTextLn "  ║ "
    c <- getInputChar "  ╚══ Execute this block? [y/n/a/q/h] "

    case c of
        Nothing -> promptCodeBlock block
        Just 'y' -> pure Execute
        Just 'n' -> pure Skip
        Just 'a' -> pure ExecuteRest
        Just 'q' -> pure SkipRest
        _ -> do
            outputTextLn helpBlock
            promptCodeBlock block

helpBlock :: Text
helpBlock = T.intercalate
    "\n"
    [ "Help:"
    , ""
    , "  y: execute this block"
    , "  n: skip this block"
    , "  a: execute this and all remaining blocks"
    , "  q: skip this and all remaining blocks"
    , "  h: show this help"
    , ""
    ]

outputTextLn :: MonadIO m => Text -> InputT m ()
outputTextLn = outputStrLn . unpack
