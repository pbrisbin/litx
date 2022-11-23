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

import Conduit
import Control.Monad.State (evalStateT, gets, modify)
import Data.Aeson
import Data.Conduit.List (sourceList)
import GHC.IO.Handle (hClose)
import LitX.CodeBlock
import LitX.Interactive (runInteractiveConduit)
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
    { eoFilter = mempty
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
    { unFilter :: CodeBlock -> All
    }
    deriving newtype (Semigroup, Monoid)

runFilter :: Filter -> CodeBlock -> Bool
runFilter f = getAll . unFilter f

filterCodeBlockTag :: Text -> Filter
filterCodeBlockTag t = Filter $ All . (== t) . codeBlockTag

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
        runInteractiveConduit $ bracketP (pure $ getStdin p) hClose $ \h -> do
            writeHeader h
            markdownSource markdown
                .| filterStatefully eoFilter
                .| (if eoInteractive then Interactive.filter p else passC)
                .| mapM_C (pushBlock h)
  where
    clearEnv = case eoInheritEnv of
        InheritEnv -> id
        Don'tInheritEnv -> setEnv []

    writeHeader h = do
        hPutStr h $ "#!" <> eoShebang <> "\n"
        traverse_ (hPutStr h . (<> "\n")) eoBanner
        traverse_ (hPutStr h . (<> "\n")) eoPreamble

    pushBlock h block = do
        hPutStr h $ "\n" <> renderCodeBlock block
        hFlush h

    renderCodeBlock :: CodeBlock -> Text
    renderCodeBlock block =
        eoCommentChars
            <> " "
            <> sourceAnnotation block
            <> codeBlockContent block

-- TODO: pragmas
data MarkdownItem
    = MarkdownPragma Filter
    | MarkdownCodeBlock CodeBlock

markdownSource :: Monad m => Markdown -> ConduitT () MarkdownItem m ()
markdownSource = sourceList . map MarkdownCodeBlock . markdownCodeBlocks
--

filterStatefully :: Monad m => Filter -> ConduitT MarkdownItem CodeBlock m ()
filterStatefully = evalStateT loop
  where
    loop = do
        mItem <- lift await

        for_ mItem $ \item -> do
            case item of
                MarkdownPragma f -> modify (<> f)
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
