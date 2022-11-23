module LitX.Interactive
    ( filter
    ) where

import LitX.Prelude hiding (filter)

import Conduit
import qualified Data.Text as T
import LitX.CodeBlock
import LitX.Interactive.Class

data InteractiveState
    = Prompting
    | Executing

data PromptResult
    = Execute
    | ExecuteRest
    | Skip
    | SkipRest
    -- | Edit

filter :: ConduitT CodeBlock CodeBlock InteractiveIO ()
filter = loop Prompting
  where
    loop = \case
        Prompting -> do
            mBlock <- await

            for_ mBlock $ \block -> do
                result <- lift $ promptCodeBlock block

                case result of
                    Execute -> yieldAndLoopWith Prompting block
                    ExecuteRest -> yieldAndLoopWith Executing block
                    Skip -> loop Prompting
                    SkipRest -> pure ()

        Executing -> passC

    yieldAndLoopWith x block = do
        lift $ outputTextLn ""
        yield block
        lift $ outputTextLn "Executed. Press any key when ready to continue."
        flip when (loop x) =<< lift (waitForAnyKey "")

promptCodeBlock :: CodeBlock -> InteractiveIO PromptResult
promptCodeBlock block = do
    outputTextLn $ "  ╔══[ " <> blockName <> " ]"
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
  where
    blockName =
        codeBlockTag block
            <> "/"
            <> pack (show $ codeBlockIndex block)
            <> ": "
            <> pack (codeBlockPath block)
            <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)

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
