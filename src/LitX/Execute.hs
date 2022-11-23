{-# LANGUAGE DerivingVia #-}

module LitX.Execute
    ( ExecuteOptions
    , defaultExecuteOptions

    -- Prefer lens access because of the 'Endo'-based Options parsing
    , shebangL
    , bannerL
    , preambleL
    , commentCharsL
    , filterL
    , execL
    , argsL
    , inheritEnvL
    , interactiveL

    -- * Component types
    , Filter
    , filterCodeBlockTag
    , InheritEnv(..)

    -- * Execution
    , executeMarkdown
    ) where

import LitX.Prelude

import Data.Aeson
import GHC.IO.Handle (hClose)
import LitX.CodeBlock
import qualified LitX.Interactive as Interactive
import LitX.Parse (Markdown, markdownCodeBlocks)
import System.Process.Typed

data ExecuteOptions = ExecuteOptions
    { eoFilter :: Filter
    , eoShebang :: Text
    , eoBanner :: Maybe Text
    , eoPreamble :: Maybe Text
    , eoCommentChars :: Text
    , eoExec :: String
    , eoArgs :: [String]
    , eoInheritEnv :: InheritEnv
    , eoInteractive :: Bool
    }
    deriving stock Generic
    deriving anyclass ToJSON

defaultExecuteOptions :: ExecuteOptions
defaultExecuteOptions = ExecuteOptions
    { eoFilter = Filter $ const False
    , eoShebang = "/usr/bin/env cat"
    , eoBanner = Nothing
    , eoPreamble = Nothing
    , eoCommentChars = "#"
    , eoExec = "cat"
    , eoArgs = []
    , eoInheritEnv = InheritEnv
    , eoInteractive = False
    }

filterL :: Lens' ExecuteOptions Filter
filterL = lens eoFilter $ \x y -> x { eoFilter = y }

shebangL :: Lens' ExecuteOptions Text
shebangL = lens eoShebang $ \x y -> x { eoShebang = y }

bannerL :: Lens' ExecuteOptions (Maybe Text)
bannerL = lens eoBanner $ \x y -> x { eoBanner = y }

preambleL :: Lens' ExecuteOptions (Maybe Text)
preambleL = lens eoPreamble $ \x y -> x { eoPreamble = y }

commentCharsL :: Lens' ExecuteOptions Text
commentCharsL = lens eoCommentChars $ \x y -> x { eoCommentChars = y }

execL :: Lens' ExecuteOptions String
execL = lens eoExec $ \x y -> x { eoExec = y }

argsL :: Lens' ExecuteOptions [String]
argsL = lens eoArgs $ \x y -> x { eoArgs = y }

inheritEnvL :: Lens' ExecuteOptions InheritEnv
inheritEnvL = lens eoInheritEnv $ \x y -> x { eoInheritEnv = y }

interactiveL :: Lens' ExecuteOptions Bool
interactiveL = lens eoInteractive $ \x y -> x { eoInteractive = y }

newtype Filter = Filter
    { runFilter :: CodeBlock -> Bool
    }

filterCodeBlockTag :: Text -> Filter
filterCodeBlockTag t = Filter $ (== t) . codeBlockTag

instance ToJSON Filter where
    toJSON _ = toJSON ()
    toEncoding _ = toEncoding ()

data InheritEnv
    = InheritEnv
    | Don'tInheritEnv
    deriving stock Generic
    deriving anyclass ToJSON

executeMarkdown :: MonadUnliftIO m => ExecuteOptions -> Markdown -> m ()
executeMarkdown ExecuteOptions {..} markdown = do
    let pc = clearEnv $ setStdin createPipe $ proc eoExec eoArgs

    withProcessWait_ pc $ \p -> do
        let h = getStdin p
            blocks = filter (runFilter eoFilter) $ markdownCodeBlocks markdown

            checkProcess = do
                mExitCode <- getExitCode p
                traverse_ (\_ -> checkExitCode p) mExitCode

            onExecute block = do
                hPutStr h $ "\n" <> renderCodeBlock block
                hFlush h

        hPutStr h $ "#!" <> eoShebang <> "\n"
        traverse_ (hPutStr h . (<> "\n")) eoBanner
        traverse_ (hPutStr h . (<> "\n")) eoPreamble

        if eoInteractive
            then Interactive.handleCodeBlocks blocks checkProcess onExecute
            else traverse_ onExecute blocks

        liftIO $ hClose h
  where
    clearEnv = case eoInheritEnv of
        InheritEnv -> id
        Don'tInheritEnv -> setEnv []

    renderCodeBlock :: CodeBlock -> Text
    renderCodeBlock block =
        eoCommentChars
            <> " "
            <> sourceAnnotation block
            <> codeBlockContent block

    sourceAnnotation :: CodeBlock -> Text
    sourceAnnotation block =
        "index="
            <> pack (show $ codeBlockIndex block)
            <> " "
            <> "source="
            <> pack (codeBlockPath block)
            <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)
            <> "\n"
