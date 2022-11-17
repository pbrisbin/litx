module LitXSpec
    ( spec
    ) where

import LitX.Prelude

import LitX
import LitX.Options (parseOptions)
import Test.Hspec

spec :: Spec
spec = do
    describe "litx" $ do
        it "processes our example correctly" $ do
            let tmp = "/tmp/litx-test.bash"
                options = ["--input", "files/examples.md", "--output", tmp]

            litx =<< parseOptions options

            actual <- readFile tmp
            expected <- readFile "files/example.bash"
            actual `shouldBe` expected
