module LitX.Execute.Options
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
    , runFilter
    , filterCodeBlockTag
    , filterCodeBlockIndexNot
    , InheritEnv(..)
    ) where

import LitX.Prelude

import LitX.CodeBlock

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

filterCodeBlockIndexNot :: Natural -> Filter
filterCodeBlockIndexNot n = Filter $ All . (/= n) . codeBlockIndex

data InheritEnv
    = InheritEnv
    | Don'tInheritEnv
