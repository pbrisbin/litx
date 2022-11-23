module LitX.Execute
    ( executeMarkdown
    , executeSource
    ) where

import LitX.Prelude

import Conduit
import Control.Monad.State (evalStateT, gets, modify)
import Data.ByteString (ByteString)
import GHC.IO.Handle (Handle, hClose)
import LitX.CodeBlock
import LitX.Execute.Options
import qualified LitX.Interactive as Interactive
import LitX.Interactive.Class (runInteractiveIO)
import LitX.Parse.Markdown (Markdown, MarkdownItem(..), markdownSource)
import System.Process.Typed

executeMarkdown :: MonadUnliftIO m => ExecuteOptions -> Markdown -> m ()
executeMarkdown options markdown = withProcessWait_ pc $ \p ->
    runInteractiveIO $ runConduit $ bracketP (pure $ getStdin p) hClose $ run p
  where
    run p h =
        executeSource options markdown (interactivity p)
            .| concatMapC textToFlush
            .| sinkHandleFlush h

    pc =
        case options ^. inheritEnvL of
                InheritEnv -> id
                Don'tInheritEnv -> setEnv []
            $ setStdin createPipe
            $ proc (options ^. execL) (options ^. argsL)

    interactivity p =
        iterMC (\_ -> checkOnProcess p)
            .| if options ^. interactiveL then Interactive.filter else passC

executeSource
    :: Monad m
    => ExecuteOptions
    -> Markdown
    -> ConduitT CodeBlock CodeBlock m ()
    -> ConduitT () Text m ()
executeSource options markdown interactivity = do
    yield $ "#!" <> options ^. shebangL <> "\n"
    traverse_ (yield . (<> "\n")) $ options ^. bannerL
    traverse_ (yield . (<> "\n")) $ options ^. preambleL
    markdownSource markdown
        .| filterStatefully (options ^. filterL)
        .| interactivity
        .| mapC (("\n" <>) . renderCodeBlock)
  where
    renderCodeBlock block =
        options
            ^. commentCharsL
            <> " "
            <> sourceAnnotation block
            <> codeBlockContent block

textToFlush :: Text -> [Flush ByteString]
textToFlush t = [Chunk $ encodeUtf8 t, Flush]

checkOnProcess :: MonadIO m => Process Handle () () -> m ()
checkOnProcess p = do
    m <- getExitCode p
    traverse_ (\_ -> checkExitCode p) m

filterStatefully :: Monad m => Filter -> ConduitT MarkdownItem CodeBlock m ()
filterStatefully = evalStateT loop
  where
    loop = do
        mItem <- lift await

        for_ mItem $ \item -> do
            case item of
                MarkdownPragma{} -> pure ()
                MarkdownPragmaError{} -> pure ()
                MarkdownFilterPragma f -> modify (<> f)
                MarkdownCodeBlock cb -> do
                    shouldYield <- gets (`runFilter` cb)
                    when shouldYield $ lift $ yield cb
            loop

sourceAnnotation :: CodeBlock -> Text
sourceAnnotation block =
    "index="
        <> pack (show $ codeBlockIndex block)
        <> " "
        <> "source="
        <> pack (codeBlockPath block)
        <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)
        <> "\n"
