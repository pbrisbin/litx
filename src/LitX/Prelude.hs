module LitX.Prelude
    ( module X

    -- * Base
    , snoc
    , hush
    , mostFrequentBy

    -- * IO
    , putStr
    , getContents
    , readFile
    , writeFile

    -- * CMark
    , walkNodes
    ) where

import Prelude as X hiding (getContents, putStr, readFile, writeFile)

import Control.Applicative as X ((<|>))
import Control.Monad as X (unless, void, when, (<=<))
import Control.Monad.IO.Class as X (MonadIO(..))
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Data.Foldable as X (fold)
import Data.Maybe as X (catMaybes, fromMaybe, listToMaybe)
import Data.Semigroup as X (First(..), Last(..), Sum(..))
import Data.Semigroup.Generic as X (GenericSemigroupMonoid(..))
import Data.String as X (IsString(..))
import Data.Text as X (Text, pack, unpack)
import Data.Text.Encoding as X (encodeUtf8)
import GHC.Generics as X (Generic)
import Lens.Micro as X (Lens', lens, (&), (.~), (<&>), (<>~), (?~), (^.))

import CMark (Node(..))
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import qualified Data.Text.IO as T

snoc :: [a] -> a -> [a]
snoc as a = as <> [a]

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

mostFrequentBy :: Ord b => (a -> b) -> [a] -> Maybe b
mostFrequentBy f =
    fmap (fst . NE.head)
        . NE.nonEmpty
        . sortOn (Down . snd)
        . Map.toList
        . Map.fromListWith (<>)
        . map pair
    where pair a = (f a, Sum @Int 1)

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
