module LitX.Interactive.Editor
    ( editCodeBlock
    ) where

import LitX.Prelude

import GHC.IO.Handle (hClose)
import LitX.CodeBlock
import System.Process.Typed
import UnliftIO.Environment (lookupEnv)
import UnliftIO.Temporary (withSystemTempFile)

editCodeBlock :: MonadUnliftIO m => CodeBlock -> m (Either Text CodeBlock)
editCodeBlock block = do
    editor <- fromMaybe "vi" <$> lookupEnv "EDITOR"

    withSystemTempFile "litx" $ \tmp h -> do
        liftIO $ hClose h
        writeFile tmp $ codeBlockContent block
        ec <- runProcess $ proc editor [tmp]

        case ec of
            ExitSuccess -> Right . updateCodeBlock block <$> readFile tmp
            ExitFailure n -> pure $ Left $ "exit " <> pack (show n)
