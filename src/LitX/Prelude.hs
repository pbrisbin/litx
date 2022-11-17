module LitX.Prelude
    ( module X

    -- * Base
    , snoc

    -- * IO
    , putStr
    , getContents
    , readFile
    , writeFile

    -- * CMark
    , walkNodes
    ) where

import Prelude as X hiding (getContents, putStr, readFile, writeFile)

import Control.Lens as X
    (Lens', Prism', lens, prism', (&), (.~), (<&>), (<>~), (?~), (^.))
import Control.Monad as X ((<=<))
import Control.Monad.IO.Class as X (MonadIO(..))
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Data.Foldable as X (fold)
import Data.Maybe as X (catMaybes, fromMaybe, listToMaybe)
import Data.Semigroup as X (Endo(..), First(..), Last(..))
import Data.Semigroup.Generic as X (GenericSemigroupMonoid(..))
import Data.String as X (IsString(..))
import Data.Text as X (Text, pack, unpack)
import Data.Text.Encoding as X (encodeUtf8)
import GHC.Generics as X (Generic)

import CMark (Node(..))
import qualified Data.Text.IO as T

snoc :: [a] -> a -> [a]
snoc as a = as <> [a]

getContents :: MonadIO m => m Text
getContents = liftIO T.getContents

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . T.putStr

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . T.readFile

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile p = liftIO . T.writeFile p

walkNodes :: (Node -> Maybe a) -> Node -> [a]
walkNodes f node =
    maybe id (flip snoc) (f node) $ concatMap (walkNodes f) $ nodeChildren node

nodeChildren :: Node -> [Node]
nodeChildren (Node _ _ children) = children
