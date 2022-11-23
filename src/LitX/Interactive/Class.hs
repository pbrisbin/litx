module LitX.Interactive.Class
    ( InteractiveIO
    , runInteractiveIO
    , outputTextLn
    , getInputChar
    , waitForAnyKey
    ) where

import LitX.Prelude

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResource(..), ResourceT, runResourceT)
import System.Console.Haskeline (InputT, defaultSettings, runInputT)
import qualified System.Console.Haskeline as Haskeline

newtype InteractiveIO a = InteractiveIO
    { unInteractiveIO :: InputT (ResourceT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        )

instance MonadResource InteractiveIO where
    liftResourceT = InteractiveIO . lift . liftResourceT

runInteractiveIO :: MonadIO m => InteractiveIO a -> m a
runInteractiveIO =
    liftIO . runResourceT . runInputT defaultSettings . unInteractiveIO

outputTextLn :: Text -> InteractiveIO ()
outputTextLn = InteractiveIO . Haskeline.outputStrLn . unpack

getInputChar :: String -> InteractiveIO (Maybe Char)
getInputChar = InteractiveIO . Haskeline.getInputChar

waitForAnyKey :: String -> InteractiveIO Bool
waitForAnyKey = InteractiveIO . Haskeline.waitForAnyKey
