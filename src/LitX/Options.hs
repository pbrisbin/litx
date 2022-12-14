module LitX.Options
    ( Options
    , optionsInput
    , optionsExecuteOptions
    , Input
    , showInput
    , inputGetContents
    , parseOptions
    , parseExecuteOptions
    , executeOptionsParser
    , getExecuteOptions
    , parsePragma
    ) where

import LitX.Prelude

import LitX.Execute.Options
import LitX.Language
import Options.Applicative

data Options = Options
    { oInput :: Input
    , oExecuteOptions :: Dual (Endo ExecuteOptions)
    }

optionsInput :: Options -> Input
optionsInput = oInput

optionsExecuteOptions :: Options -> Dual (Endo ExecuteOptions)
optionsExecuteOptions = oExecuteOptions

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

inputGetContents :: MonadIO m => Input -> m Text
inputGetContents = \case
    InputStdin -> getContents
    InputFile path -> readFile path


getExecuteOptions :: Dual (Endo ExecuteOptions) -> ExecuteOptions
getExecuteOptions f = appEndo (getDual f) defaultExecuteOptions

parseOptions :: MonadIO m => [String] -> m Options
parseOptions = parseArgs optionsParser

parseExecuteOptions :: MonadIO m => [String] -> m (Dual (Endo ExecuteOptions))
parseExecuteOptions = parseArgs executeOptionsParser

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strOption
                (mconcat
                    [ short 'i'
                    , long "input"
                    , metavar "PATH|-"
                    , help "Read Markdown from PATH"
                    , value InputStdin
                    , showDefaultWith showInput
                    , action "file"
                    ]
                )
        <*> executeOptionsParser

executeOptionsParser :: Parser (Dual (Endo ExecuteOptions))
executeOptionsParser =
    mconcat <$> sequenceA (languageOptionParsers <> executeOptionsParsers)

languageOptionParsers :: [Parser (Dual (Endo ExecuteOptions))]
languageOptionParsers = map languageOptionParser [minBound .. maxBound]

-- brittany-disable-next-binding

executeOptionsParsers :: [Parser (Dual (Endo ExecuteOptions))]
executeOptionsParsers =
    [ eOptional ((filterL .~) . filterCodeBlockTag) strOption $ mconcat
        [ long "filter-tag"
        , metavar "TEXT"
        , help "Filter to code-blocks of the given tag"
        ]
    , eOptional (shebangL .~) strOption $ mconcat
        [ long "shebang"
        , metavar "TEXT"
        , help "Set the shebang (without '#!')"
        ]
    , eSwitch (bannerL .~ Nothing) $ mconcat
        [ long "no-banner"
        , help "Disable any 'Generated by...' banner"]
    , eOptional (bannerL ?~) strOption $ mconcat
        [ long "banner"
        , metavar "TEXT", help "Set the banner"]
    , eSwitch (preambleL .~ Nothing) $ mconcat
        [ long "no-preamble"
        , help "Disable any preamble"]
    , eOptional (preambleL ?~) strOption $ mconcat
        [ long "preamble"
        , metavar "TEXT"
        , help "Set the preamble"]
    , eOptional (commentCharsL .~) strOption $ mconcat
        [ long "comment-chars"
        , metavar "TEXT"
        , help "Set the characters used for line comments"
        ]
    , eOptional setExec strOption $ mconcat
        [ long "exec"
        , metavar "CMD"
        , help "Execute script using CMD"
        ]
    , eMany (argsL <>~) strOption $ mconcat
        [ long "arg"
        , help "Pass additional arguments when executing"
        , metavar "ARG"
        ]
    , eFlag (inheritEnvL .~) InheritEnv Don'tInheritEnv $ mconcat
        [ long "no-env"
        , help "Don't inherit ENV in the executed process"
        ]
    , eSwitch (interactiveL .~ True) $ mconcat
        [ long "interactive"
        , help "Prompt before executing each code-block"
        ]
    ]

setExec :: String -> ExecuteOptions -> ExecuteOptions
setExec cmd eo = eo & execL .~ cmd & argsL .~ []

parseArgs :: MonadIO m => Parser a -> [String] -> m a
parseArgs parser = liftIO . handleParseResult . execParserPure defaultPrefs p
  where
    p =
        info (parser <**> helper)
            $ progDesc "Execute Literate Markdown programs"
            <> fullDesc
            <> footer "See litx(1) for more details."

parsePragma :: Parser a -> [String] -> Either String a
parsePragma parser = fromParseResult . execParserPure defaultPrefs p
  where
    p = info (parser <**> helper) fullDesc
    fromParseResult = \case
        Success a -> Right a
        Failure f -> Left $ show f
        CompletionInvoked{} -> Left "Unexpected completion"

eOptional
    :: (a -> b -> b)
    -> (Mod OptionFields a -> Parser a)
    -> Mod OptionFields a
    -> Parser (Dual (Endo b))
eOptional f o m = maybe mempty (Dual . Endo . f) <$> optional (o m)

eMany
    :: ([a] -> b -> b)
    -> (Mod OptionFields a -> Parser a)
    -> Mod OptionFields a
    -> Parser (Dual (Endo b))
eMany f o m = Dual . Endo . f <$> many (o m)

eSwitch :: (a -> a) -> Mod FlagFields (Dual (Endo a)) -> Parser (Dual (Endo a))
eSwitch f = flag mempty (Dual $ Endo f)

eFlag :: (a -> b -> b) -> a -> a -> Mod FlagFields a -> Parser (Dual (Endo b))
eFlag f inactive active = fmap (Dual . Endo . f) . flag inactive active
