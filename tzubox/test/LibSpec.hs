module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck


import qualified UserConfig as UC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parsing config files" $ do
    it "parsing paths and masterip should work" $ do
      let line1 = "path = a"
      let line2 = "path = ab"
      let line3 = "masterip = c"
      UC.parseLine line1 `shouldBe` (Just $ UC.ParsedPath $ UC.Path "a")
      UC.parseLine line2 `shouldBe` (Just $ UC.ParsedPath $ UC.Path "ab")
      UC.parseLine line3 `shouldBe` (Just $ UC.ParsedMasterIp $ UC.MasterIp "c")
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
