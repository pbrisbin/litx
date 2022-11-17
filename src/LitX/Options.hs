{-# LANGUAGE DerivingVia #-}

module LitX.Options
    ( Options(..)
    , Input(..)
    , parseOptions
    ) where

import LitX.Prelude

import LitX.Execute
import LitX.Language
import Options.Applicative

data Options = Options
    { oInput :: Last Input
    , oLanguage :: Last Language
    , oModExecuteOptions :: Endo ExecuteOptions
    }
    deriving stock Generic
    deriving Semigroup via GenericSemigroupMonoid Options

data Input
    = InputStdin
    | InputFile FilePath

instance IsString Input where
    fromString = \case
        "-" -> InputStdin
        path -> InputFile path

showInput :: Input -> String
showInput = \case
    InputStdin -> "-"
    InputFile path -> path

parseOptions :: MonadIO m => [String] -> m Options
parseOptions = liftIO . handleParseResult . execParserPure defaultPrefs p
  where
    p =
        info (parser <**> helper)
            $ progDesc "Execute Literate Markdown programs"
            <> fullDesc

-- brittany-disable-next-binding

parser :: Parser Options
parser = Options
    <$> (Last <$> strOption
        (  short 'i'
        <> long "input"
        <> metavar "PATH|-"
        <> help "Read Markdown from PATH"
        <> value InputStdin
        <> showDefaultWith showInput
        ))
    <*> (Last <$> langOption
        (  short 'l'
        <> long "language"
        <> help "Extract and execute code blocks in this language"
        ))
    <*> (mconcat <$> sequenceA
        [ optionalEndo setOutput (strOption
            (  short 'o'
            <> long "output"
            <> metavar "PATH|-"
            <> help "Output script to PATH instead of execution"
            ))
        , optionalEndo (outputL .~) (strOption
            (  short 's'
            <> long "save"
            <> metavar "PATH|-"
            <> help "Save script to PATH prior to execution"
            ))
        , optionalEndo setExec (strOption
            (  long "exec"
            <> metavar "CMD"
            <> help "Execute script using CMD"
            ))
        , manyEndo (executeArgsL <>~) (strOption
            (  long "exec-arg"
            <> help "Pass additional arguments when executing"
            <> metavar "ARG"
            ))
        , optionalEndo (shebangL .~) (strOption
            (  long "shebang"
            <> metavar "TEXT"
            <> help "Change the shebang"
            ))
        , optionalEndo (bannerL .~) (strOption
            (  long "banner"
            <> metavar "TEXT"
            <> help "Change the 'Generated by...' banner"
            ))
        , optionalEndo (preambleL .~) (strOption
            (  long "preamble"
            <> metavar "TEXT"
            <> help "Change the preamble (e.g. 'set -e')"
            ))
        ])

setOutput :: Output -> ExecuteOptions -> ExecuteOptions
setOutput o eo = eo & outputL .~ o & executeModeL .~ NoExecute

setExec :: String -> ExecuteOptions -> ExecuteOptions
setExec cmd eo = eo & executeModeL . _Execute .~ cmd & executeArgsL .~ []

optionalEndo :: (a -> b -> b) -> Parser a -> Parser (Endo b)
optionalEndo f p = maybe mempty (Endo . f) <$> optional p

manyEndo :: ([a] -> b -> b) -> Parser a -> Parser (Endo b)
manyEndo f p = Endo . f <$> many p