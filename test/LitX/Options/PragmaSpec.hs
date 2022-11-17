module LitX.Options.PragmaSpec
    ( spec
    ) where

import LitX.Prelude

import CMark
import qualified Data.Text as T
import LitX.Options.Pragma
import Test.Hspec

spec :: Spec
spec = do
    describe "getPragmaArgs" $ do
        it "parses a single-line comment" $ do
            let
                markdown = T.unlines
                    [ "<!-- litx --foo --bar=\"baz bat\" -->"
                    , ""
                    , "# My Markdown Document"
                    , ""
                    , "```bash"
                    , "echo \"hi\""
                    , "```"
                    ]

            getPragmaArgs' markdown `shouldBe` Just ["--foo", "--bar=baz bat"]

        it "parses a multi-line comment" $ do
            let
                markdown = T.unlines
                    [ "<!--"
                    , "  litx"
                    , "    --foo"
                    , "    --bar=\"baz bat\""
                    , "-->"
                    , ""
                    , "# My Markdown Document"
                    , ""
                    , "```bash"
                    , "echo \"hi\""
                    , "```"
                    ]

            getPragmaArgs' markdown `shouldBe` Just ["--foo", "--bar=baz bat"]

        it "parses the entire comment contents" $ do
            let
                markdown = T.unlines
                    [ "<!-- litx --foo --bar=\"baz bat\""
                    , ""
                    , " Oops, this is unexpected. -->"
                    , ""
                    , "# My Markdown Document"
                    , ""
                    , "```bash"
                    , "echo \"hi\""
                    , "```"
                    ]

            -- This will fail, hopefully informatively, when it gets to
            -- parseOptions
            getPragmaArgs' markdown
                `shouldBe` Just
                               [ "--foo"
                               , "--bar=baz bat"
                               , "Oops,"
                               , "this"
                               , "is"
                               , "unexpected."
                               ]

getPragmaArgs' :: Text -> Maybe [String]
getPragmaArgs' = getPragmaArgs . commonmarkToNode []
