module LitX.Prelude
    ( module X

    -- * Base
    , snoc
    , hush
    , note
    , mostFrequent
    , mostFrequentBy
    , fromMaybeM

    -- * IO
    , putStr
    , putStrLn
    , getContents
    , getLine
    , hPutStr
    , hPutStrLn
    , hFlush
    , readFile
    , writeFile

    -- * CMark
    , walkNodes

    -- * Conduit
    , passC
    ) where

import Prelude as X hiding
    (getContents, getLine, putStr, putStrLn, readFile, writeFile)

import Control.Applicative as X ((<|>))
import Control.Arrow as X ((&&&), (***))
import Control.Monad as X (unless, void, when, (<=<))
import Control.Monad.IO.Class as X (MonadIO(..))
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Data.Foldable as X (fold, for_, traverse_)
import Data.Maybe as X (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Semigroup as X
    (All(..), Dual(..), Endo(..), First(..), Last(..), Sum(..))
import Data.Semigroup.Generic as X (GenericSemigroupMonoid(..))
import Data.String as X (IsString(..))
import Data.Text as X (Text, pack, unpack)
import Data.Text.Encoding as X (encodeUtf8)
import GHC.Generics as X (Generic)
import Lens.Micro as X (Lens', lens, (&), (.~), (<&>), (<>~), (?~), (^.))
import Numeric.Natural as X (Natural)

import CMark (Node(..))
import Conduit (ConduitT, awaitForever, yield)
import Control.Monad.State (evalState, get, modify)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import qualified Data.Text.IO as T
import GHC.IO.Handle (Handle)
import qualified System.IO as IO

snoc :: [a] -> a -> [a]
snoc as a = as <> [a]

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

mostFrequent :: Ord a => [a] -> Maybe a
mostFrequent = mostFrequentBy id

mostFrequentBy :: Ord b => (a -> b) -> [a] -> Maybe b
mostFrequentBy f =
    fmap (fst . NE.head)
        . NE.nonEmpty
        . sortOn (Down . snd)
        . Map.toList
        . Map.fromListWith (<>)
        . map pair
    where pair a = (f a, Sum @Int 1)

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM ma mma = maybe ma pure =<< mma

getContents :: MonadIO m => m Text
getContents = liftIO T.getContents

getLine :: MonadIO m => m Text
getLine = liftIO T.getLine

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . T.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . T.putStrLn

hPutStr :: MonadIO m => Handle -> Text -> m ()
hPutStr h = liftIO . T.hPutStr h

hFlush :: MonadIO m => Handle -> m ()
hFlush = liftIO . IO.hFlush

hPutStrLn :: MonadIO m => Handle -> Text -> m ()
hPutStrLn h = liftIO . T.hPutStrLn h

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . T.readFile

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile p = liftIO . T.writeFile p

walkNodes :: (Natural -> Node -> Maybe a) -> Node -> [a]
walkNodes f = flip evalState 0 . go
  where
    go node = do
        n <- get
        as <- case f n node of
            Nothing -> pure []
            Just a -> [a] <$ modify (+ 1)

        rest <- concat <$> traverse go (nodeChildren node)
        pure $ as <> rest

nodeChildren :: Node -> [Node]
nodeChildren (Node _ _ children) = children

passC :: Monad m => ConduitT i i m ()
passC = awaitForever yield
