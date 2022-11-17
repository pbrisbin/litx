module LitX.CodeBlock
    ( CodeBlock
    , getCodeBlocks
    , renderCodeBlocks
    ) where

import LitX.Prelude

import CMark
import LitX.Options (CodeBlockTag(..))

data CodeBlock

getCodeBlocks :: CodeBlockTag -> Node -> [CodeBlock]
getCodeBlocks = undefined

renderCodeBlocks :: [CodeBlock] -> Text
renderCodeBlocks = undefined
