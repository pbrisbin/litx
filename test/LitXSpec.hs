module LitXSpec
    ( spec
    ) where

import LitX.Prelude

import LitX
import Test.Hspec

spec :: Spec
spec = do
    describe "litx" $ do
        it "processes our example correctly" $ do
            let tmp = "/tmp/litx-test.bash"

            litx $ concat
                [ ["--input", "files/example.md"]
                , ["--exec", "sh"]
                , ["--arg", "-c"]
                , ["--arg", "cat > '" <> tmp <> "'"]
                ]

            actual <- readFile tmp
            expected <- readFile "files/example.bash"
            actual `shouldBe` expected
