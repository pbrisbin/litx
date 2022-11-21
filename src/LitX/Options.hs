module LitX.Options
    ( Options
    , optionsInput
    , optionsExecuteOptions
    , parseOptions
    , parseExecuteOptions
    ) where

import LitX.Prelude

import LitX.Execute
import LitX.Language
import LitX.Parse
import Options.Applicative

data Options = Options
    { oInput :: Input
    , oExecuteOptions :: Dual (Endo ExecuteOptions)
    }

optionsInput :: Options -> Input
optionsInput = oInput

optionsExecuteOptions :: Options -> Dual (Endo ExecuteOptions)
optionsExecuteOptions = oExecuteOptions

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
    [ eOptional (commentCharsL .~) strOption $ mconcat
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

eFlag :: (a -> b -> b) -> a -> a -> Mod FlagFields a -> Parser (Dual (Endo b))
eFlag f inactive active = fmap (Dual . Endo . f) . flag inactive active
