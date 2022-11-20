module LitXSpec
    ( spec
    ) where

import LitX.Prelude

import qualified Data.Text as T
import Data.Version
import LitX
import qualified Paths_litx as Pkg
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
            expected <- replaceVersion <$> readFile "files/example.bash"
            actual `shouldBe` expected

replaceVersion :: Text -> Text
replaceVersion = T.replace "{version}" (pack $ showVersion Pkg.version)
