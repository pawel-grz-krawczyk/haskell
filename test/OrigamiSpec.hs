module OrigamiSpec(spec) where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =  do
  describe "Origami" $ do
    it "foo" $ do
      1 `shouldBe` 1


