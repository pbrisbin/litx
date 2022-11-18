module LitXSpec
    ( spec
    ) where

import LitX.Prelude

import qualified Data.Text as T
import Data.Version
import LitX
import qualified Paths_litx as Pkg
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
            expected <- replaceVersion <$> readFile "files/example.bash"
            actual `shouldBe` expected

replaceVersion :: Text -> Text
replaceVersion = T.replace "{version}" (pack $ showVersion Pkg.version)
