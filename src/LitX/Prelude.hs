module LitX.Prelude
    ( module X
    , putStr
    , getContents
    , readFile
    , writeFile
    ) where

import Prelude as X hiding (getContents, putStr, readFile, writeFile)

import Control.Monad.IO.Class as X (MonadIO(..))
import Data.Text as X (Text, pack, unpack)

import qualified Data.Text.IO as T

getContents :: MonadIO m => m Text
getContents = liftIO T.getContents

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . T.putStr

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . T.readFile

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile p = liftIO . T.writeFile p
