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
    , executeModeL
    , executeArgsL
    , inheritEnvL

    -- * Component types
    , Filter(..)
    , ExecuteMode(..)
    , InheritEnv(..)

    -- * Execution
    , executeMarkdown
    ) where

import LitX.Prelude

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import LitX.CodeBlock
import LitX.Parse (Markdown, markdownCodeBlocks)
import System.Process.Typed

data ExecuteOptions = ExecuteOptions
    { eoFilter :: Filter
    , eoShebang :: Text
    , eoBanner :: Maybe Text
    , eoPreamble :: Maybe Text
    , eoCommentChars :: Text
    , eoExecuteMode :: ExecuteMode
    , eoExecuteArgs :: [String]
    , eoInheritEnv :: InheritEnv
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
    , eoExecuteMode = Execute "cat"
    , eoExecuteArgs = []
    , eoInheritEnv = InheritEnv
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

executeModeL :: Lens' ExecuteOptions ExecuteMode
executeModeL = lens eoExecuteMode $ \x y -> x { eoExecuteMode = y }

executeArgsL :: Lens' ExecuteOptions [String]
executeArgsL = lens eoExecuteArgs $ \x y -> x { eoExecuteArgs = y }

inheritEnvL :: Lens' ExecuteOptions InheritEnv
inheritEnvL = lens eoInheritEnv $ \x y -> x { eoInheritEnv = y }

newtype Filter = Filter
    { runFilter :: CodeBlock -> Bool
    }

instance ToJSON Filter where
    toJSON _ = toJSON ()
    toEncoding _ = toEncoding ()

data ExecuteMode
    = Execute String
    | NoExecute
    deriving stock Generic
    deriving anyclass ToJSON

data InheritEnv
    = InheritEnv
    | Don'tInheritEnv
    deriving stock Generic
    deriving anyclass ToJSON

executeMarkdown :: MonadUnliftIO m => ExecuteOptions -> Markdown -> m ()
executeMarkdown ExecuteOptions {..} markdown = do
    case eoExecuteMode of
        Execute cmd -> do
            let input = byteStringInput $ BSL.fromStrict $! encodeUtf8 script
            runProcess_ $ clearEnv $ setStdin input $ proc cmd eoExecuteArgs
        NoExecute -> pure ()
  where
    script = mconcat
        [ "#!" <> eoShebang <> "\n"
        , maybe "" (<> "\n") eoBanner
        , maybe "" (<> "\n\n") eoPreamble
        , renderCodeBlocks
        ]

    clearEnv = case eoInheritEnv of
        InheritEnv -> id
        Don'tInheritEnv -> setEnv []

    renderCodeBlocks :: Text
    renderCodeBlocks =
        T.intercalate "\n"
            $ map renderCodeBlock
            $ filter (runFilter eoFilter)
            $ markdownCodeBlocks markdown

    renderCodeBlock :: CodeBlock -> Text
    renderCodeBlock block =
        eoCommentChars
            <> " "
            <> sourceAnnotation block
            <> codeBlockContent block

    sourceAnnotation :: CodeBlock -> Text
    sourceAnnotation block =
        "source="
            <> pack (codeBlockPath block)
            <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)
            <> "\n"
