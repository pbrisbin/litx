{-# LANGUAGE DerivingVia #-}

module LitX.Execute
    ( ExecuteOptions
    , getExecuteOptions

    -- Prefer lens access because of the 'Endo'-based Options parsing
    , commentCharsL
    , filterL
    , execL
    , argsL
    , inheritEnvL

    -- * Component types
    , Filter(..)
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
    , eoCommentChars :: Text
    , eoExec :: String
    , eoArgs :: [String]
    , eoInheritEnv :: InheritEnv
    }
    deriving stock Generic
    deriving anyclass ToJSON

getExecuteOptions :: Dual (Endo ExecuteOptions) -> ExecuteOptions
getExecuteOptions f = appEndo (getDual f) $ ExecuteOptions
    { eoFilter = Filter $ const False
    , eoCommentChars = "#"
    , eoExec = "cat"
    , eoArgs = []
    , eoInheritEnv = InheritEnv
    }

filterL :: Lens' ExecuteOptions Filter
filterL = lens eoFilter $ \x y -> x { eoFilter = y }

commentCharsL :: Lens' ExecuteOptions Text
commentCharsL = lens eoCommentChars $ \x y -> x { eoCommentChars = y }

execL :: Lens' ExecuteOptions String
execL = lens eoExec $ \x y -> x { eoExec = y }

argsL :: Lens' ExecuteOptions [String]
argsL = lens eoArgs $ \x y -> x { eoArgs = y }

inheritEnvL :: Lens' ExecuteOptions InheritEnv
inheritEnvL = lens eoInheritEnv $ \x y -> x { eoInheritEnv = y }

newtype Filter = Filter
    { runFilter :: CodeBlock -> Bool
    }

instance ToJSON Filter where
    toJSON _ = toJSON ()
    toEncoding _ = toEncoding ()

data InheritEnv
    = InheritEnv
    | Don'tInheritEnv
    deriving stock Generic
    deriving anyclass ToJSON

executeMarkdown :: MonadUnliftIO m => ExecuteOptions -> Markdown -> m ()
executeMarkdown options@ExecuteOptions {..} markdown = do
    let
        input =
            byteStringInput
                $ BSL.fromStrict
                $! encodeUtf8
                $ T.intercalate "\n"
                $ map (renderCodeBlock options)
                $ filter (runFilter eoFilter)
                $ markdownCodeBlocks markdown

    runProcess_ $ clearEnv $ setStdin input $ proc eoExec eoArgs
  where
    clearEnv = case eoInheritEnv of
        InheritEnv -> id
        Don'tInheritEnv -> setEnv []

renderCodeBlock :: ExecuteOptions -> CodeBlock -> Text
renderCodeBlock ExecuteOptions {..} block =
    eoCommentChars <> " " <> sourceAnnotation block <> codeBlockContent block

sourceAnnotation :: CodeBlock -> Text
sourceAnnotation block =
    "source="
        <> pack (codeBlockPath block)
        <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)
        <> "\n"
