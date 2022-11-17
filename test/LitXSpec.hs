module LitXSpec
    ( spec
    ) where

import LitX.Prelude

import LitX
import Test.Hspec
import UnliftIO.Directory (doesFileExist, removeFile)

spec :: Spec
spec = do
    describe "litx" $ do
        it "processes our example correctly" $ do
            let tmp = "/tmp/litx-test.bash"
            exists <- doesFileExist tmp
            when exists $ removeFile tmp

            litx ["--input", "files/example.md", "--output", tmp]

            actual <- readFile tmp
            expected <- readFile "files/example.bash"
            actual `shouldBe` expected
