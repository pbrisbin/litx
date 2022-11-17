module LitX
    ( litx
    ) where

import LitX.Prelude

import CMark
import LitX.CodeBlock
import LitX.Options

litx :: MonadIO m => Options -> m ()
litx options = do
    input <- case oInput options of
        InputStdin -> getContents
        InputFile p -> readFile p

    let node = commonmarkToNode cmarkOptions input

    Options {..} <- addPragmaOptions node options

    let blocks = getCodeBlocks oCodeBlockTag node
        script =
            renderShebang oShebang
                <> "\n"
                <> unPreamble oPreamble
                <> "\n\n"
                <> renderCodeBlocks blocks

    case oExecutionMode of
        ExecuteStdin{} -> undefined -- {script} | {cmd} {arg...}
        ExecuteProcess{} -> undefined -- tempfile + {cmd} {arg...}
        NoExecute output -> case output of
            OutputNone -> pure ()
            OutputStdout -> putStr script
            OutputFile p -> writeFile p script

cmarkOptions :: [CMarkOption]
cmarkOptions = []
