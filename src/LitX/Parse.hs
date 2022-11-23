module LitX.Parse
    ( parseMarkdown
    ) where

import LitX.Prelude

import CMark
import Control.Monad.State (MonadState, evalState, get, gets, modify)
import qualified Data.Text as T
import LitX.CodeBlock
import LitX.Execute.Options
import LitX.Options
import LitX.Parse.Markdown
import qualified ShellWords

parseMarkdown :: MonadIO m => Input -> m Markdown
parseMarkdown input = do
    node <- commonmarkToNode [] <$> inputGetContents input
    pure $ evalState (foldMapNodeM (nodeToMarkdown src) node) 0
    where src = showInput input

foldMapNodeM :: (Monad m, Monoid w) => (Node -> m w) -> Node -> m w
foldMapNodeM f node@(Node _ _ children) = foldMapM f $ node : children

nodeToMarkdown :: MonadState Natural m => FilePath -> Node -> m Markdown
nodeToMarkdown path = \case
    Node mPosInfo (CODE_BLOCK info content) _ -> do
        n <- get <* modify (+ 1)
        pure $ Markdown
            [ MarkdownCodeBlock
                  $ codeBlock n info path (startLine <$> mPosInfo) content
            ]

    Node _ (HTML_BLOCK html) _ -> htmlToLitX html
    Node _ (HTML_INLINE html) _ -> htmlToLitX html
    Node{} -> pure mempty

htmlToLitX :: MonadState Natural m => Text -> m Markdown
htmlToLitX html = do
    let args :: [String]
        args = fromMaybe [] $ do
            inner <- T.strip <$> stripAround "<!--" "-->" (T.strip html)
            hush $ ShellWords.parse $ unpack inner

    case args of
        ("litx" : rest) ->
            pure $ Markdown $ case parsePragma executeOptionsParser rest of
                Left err -> [MarkdownPragmaError rest err]
                Right os -> [MarkdownPragma rest os]
        ["litx-ignore-next"] ->
            gets
                $ Markdown
                . pure
                . MarkdownFilterPragma
                . filterCodeBlockIndexNot

        _ -> pure mempty

stripAround :: Text -> Text -> Text -> Maybe Text
stripAround prefix suffix = T.stripPrefix prefix <=< T.stripSuffix suffix
