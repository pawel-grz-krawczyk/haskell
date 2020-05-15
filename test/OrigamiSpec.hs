module OrigamiSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec =  do
  describe "Origami" $ do
    it "foo" $ do
      1 `shouldBe` 1


