{-# OPTIONS_GHC -Wno-orphans #-}

module LitX.Interactive
    ( runInteractiveConduit
    , filter
    ) where

import LitX.Prelude hiding (filter)

import Conduit
import Control.Monad.Trans.Resource (MonadResource(..))
import qualified Data.Text as T
import GHC.IO.Handle (Handle)
import LitX.CodeBlock
import System.Console.Haskeline
import System.Process.Typed (Process, checkExitCode, getExitCode)

instance MonadResource m => MonadResource (InputT m) where
    liftResourceT = lift . liftResourceT

runInteractiveConduit
    :: MonadIO m => ConduitT () Void (InputT (ResourceT IO)) r -> m r
runInteractiveConduit =
    liftIO . runResourceT . runInputT defaultSettings . runConduit

data InteractiveState
    = Prompting
    | Executing

data PromptResult
    = Execute
    | ExecuteRest
    | Skip
    | SkipRest
    -- | Edit

filter
    :: Process Handle () ()
    -> ConduitT CodeBlock CodeBlock (InputT (ResourceT IO)) ()
filter p = loop Prompting
  where
    loop = \case
        Prompting -> do
            mBlock <- await

            for_ mBlock $ \block -> do
                checkOnProcess p

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
        lift $ outputTextLn "Executed. Press any key to continue."
        flip when (loop x) =<< lift (waitForAnyKey "")

checkOnProcess :: MonadIO m => Process Handle () () -> m ()
checkOnProcess p = do
    traverse_ (\_ -> checkExitCode p) =<< getExitCode p

promptCodeBlock :: CodeBlock -> InputT (ResourceT IO) PromptResult
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
