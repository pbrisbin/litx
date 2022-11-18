module LitX.Render
    ( RenderOptions
    , defaultRenderOptions
    , renderCodeBlocks
    ) where

import LitX.Prelude

import qualified Data.Text as T
import LitX.CodeBlock
import LitX.Language

data RenderOptions = RenderOptions
    { roShebang :: Text
    , roBanner :: Text
    , roPreamble :: Text
    , roComment :: Text -> Text
    , roLanguage :: Language
    }

defaultRenderOptions :: RenderOptions
defaultRenderOptions = undefined

renderCodeBlocks :: RenderOptions -> [CodeBlock] -> Text
renderCodeBlocks RenderOptions {..} blocks = mconcat
    [ roShebang
    , roBanner
    , roPreamble
    , T.intercalate "\n" $ map (renderCodeBlock roComment) $ filter
        ((== roLanguage) . codeBlockLanguage)
        blocks
    ]

renderCodeBlock :: (Text -> Text) -> CodeBlock -> Text
renderCodeBlock comment block = comment sourceAnnotation
    <> codeBlockContent block
  where
    sourceAnnotation =
        "source="
            <> pack (codeBlockPath block)
            <> maybe "" ((":" <>) . pack . show) (codeBlockLine block)
            <> "\n"
