{-# LANGUAGE DerivingVia #-}

module LitX.Execute
    ( ExecuteOptions(..)
    , ExecuteMode(..)
    , InheritEnv(..)
    , Output(..)
    , executeScript

    -- * Ease construction of @'Endo' 'ExecuteOptions'@ in options parsing
    , shebangL
    , bannerL
    , preambleL
    , executeModeL
    , executeArgsL
    , inheritEnvL
    , outputL
    ) where

import LitX.Prelude

import qualified Data.ByteString.Lazy as BSL
import System.Process.Typed

data ExecuteOptions = ExecuteOptions
    { eoShebang :: Text
    , eoBanner :: Text
    , eoPreamble :: Text
    , eoExecuteMode :: ExecuteMode
    , eoExecuteArgs :: [String]
    , eoInheritEnv :: InheritEnv
    , eoOutput :: Output
    }

shebangL :: Lens' ExecuteOptions Text
shebangL = lens eoShebang $ \x y -> x { eoShebang = y }

bannerL :: Lens' ExecuteOptions Text
bannerL = lens eoBanner $ \x y -> x { eoBanner = y }

preambleL :: Lens' ExecuteOptions Text
preambleL = lens eoPreamble $ \x y -> x { eoPreamble = y }

executeModeL :: Lens' ExecuteOptions ExecuteMode
executeModeL = lens eoExecuteMode $ \x y -> x { eoExecuteMode = y }

executeArgsL :: Lens' ExecuteOptions [String]
executeArgsL = lens eoExecuteArgs $ \x y -> x { eoExecuteArgs = y }

inheritEnvL :: Lens' ExecuteOptions InheritEnv
inheritEnvL = lens eoInheritEnv $ \x y -> x { eoInheritEnv = y }

outputL :: Lens' ExecuteOptions Output
outputL = lens eoOutput $ \x y -> x { eoOutput = y }

data ExecuteMode
    = Execute String
    | NoExecute

data InheritEnv
    = InheritEnv
    | Don'tInheritEnv

data Output
    = OutputStdout
    | OutputFile FilePath

instance IsString Output where
    fromString = \case
        "-" -> OutputStdout
        path -> OutputFile path

executeScript :: MonadUnliftIO m => ExecuteOptions -> Text -> m ()
executeScript ExecuteOptions {..} body = do
    case eoOutput of
        OutputStdout -> putStr script
        OutputFile path -> writeFile path script

    case eoExecuteMode of
        Execute cmd -> do
            let bs = BSL.fromStrict $! encodeUtf8 script

            runProcess_ $ clearEnv $ setStdin (byteStringInput bs) $ proc
                cmd
                eoExecuteArgs

        NoExecute -> pure ()
  where
    script = "#!" <> eoShebang <> eoBanner <> eoPreamble <> "\n\n" <> body

    clearEnv = case eoInheritEnv of
        InheritEnv -> id
        Don'tInheritEnv -> setEnv []
