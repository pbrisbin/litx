module LitX.Options.Pragma
    ( getPragmaArgs
    ) where

import LitX.Prelude

import CMark
import qualified Data.Text as T
import qualified ShellWords

getPragmaArgs :: Node -> Maybe [String]
getPragmaArgs = listToMaybe . walkNodes nodeToLitX

nodeToLitX :: Node -> Maybe [String]
nodeToLitX = \case
    Node _ (HTML_BLOCK html) _ -> htmlToLitX html
    Node _ (HTML_INLINE html) _ -> htmlToLitX html
    _ -> Nothing

htmlToLitX :: Text -> Maybe [String]
htmlToLitX html = do
    inner <- T.strip <$> stripAround "<!--" "-->" (T.strip html)
    flags <- T.strip <$> T.stripPrefix "litx" inner
    hush $ ShellWords.parse $ unpack flags

stripAround :: Text -> Text -> Text -> Maybe Text
stripAround prefix suffix = T.stripPrefix prefix <=< T.stripSuffix suffix

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
