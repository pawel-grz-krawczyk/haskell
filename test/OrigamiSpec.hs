module OrigamiSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Origami

spec :: Spec
spec =  do
  describe "Origami - circle folding" $ do
    let circle = Origami.circle (0, 0) 10
    describe "without folding" $ do
      it "one layer in circle" $ do
        circle (0, 0) `shouldBe` 1
        circle (0, 10) `shouldBe` 1
        circle (0, -10) `shouldBe` 1
        circle (-10, 0) `shouldBe` 1
        circle (10, 0) `shouldBe` 1
      it "zero layers outside circle" $ do
        circle (0, 11) `shouldBe` 0
        circle (0, -11) `shouldBe` 0
        circle (-11, 0) `shouldBe` 0
        circle (11, 0) `shouldBe` 0

    describe "folding once - vertical line" $ do
      let fc = Origami.foldMany [((0, 0), (0, 10))] circle
      it "one layer on the line" $ do
         fc (0, 0) `shouldBe` 1
         fc (0, 10) `shouldBe` 1
         fc (0, -10) `shouldBe` 1
      it "two layers on the left side" $ do
         fc (-10, 0) `shouldBe` 2
         fc (-5, 2) `shouldBe` 2
      it "zero layers of the right side" $ do
         fc (5, 2) `shouldBe` 0
         fc (10, 0) `shouldBe` 0

    describe "folding once - horizontal line" $ do
      let fc = Origami.foldMany [((0, 0), (10, 0))] circle
      it "one layer on the line" $ do
         fc (0, 0) `shouldBe` 1
         fc (-10, 0) `shouldBe` 1
         fc (10, 0) `shouldBe` 1
      it "two layers on the left side" $ do
         fc (0, 10) `shouldBe` 2
         fc (-5, 2) `shouldBe` 2
         fc (5, 2) `shouldBe` 2
      it "zero layers of the right side" $ do
         fc (-5, -2) `shouldBe` 0
         fc (5, -2) `shouldBe` 0
         fc (0, -10) `shouldBe` 0

    describe "folding once - f(x) = x" $ do
      let fc = Origami.foldMany [((0, 0), (1, 1))] circle
      it "one layer on the line" $ do
         fc (0, 0) `shouldBe` 1
         fc (-5, -5) `shouldBe` 1
         fc (5, 5) `shouldBe` 1
      it "two layers on the left side" $ do
         fc (0, 0.5) `shouldBe` 2
         fc (0, 10) `shouldBe` 2
         fc (-10, 0) `shouldBe` 2
      it "zero layers of the right side" $ do
         fc (0, -0.5) `shouldBe` 0
         fc (0, -10) `shouldBe` 0
         fc (10, 0) `shouldBe` 0

    describe "folding once - f(x) = -x" $ do
      let fc = Origami.foldMany [((0, 0), (1, -1))] circle
      it "one layer on the line" $ do
         fc (0, 0) `shouldBe` 1
         fc (-5, 5) `shouldBe` 1
         fc (5, -5) `shouldBe` 1
      it "two layers on the left side" $ do
         fc (0, 0.5) `shouldBe` 2
         fc (0, 10) `shouldBe` 2
         fc (10, 0) `shouldBe` 2
      it "zero layers of the right side" $ do
         fc (0, -0.5) `shouldBe` 0
         fc (0, -10) `shouldBe` 0
         fc (-10, 0) `shouldBe` 0

    describe "folding once - f(x) = -x with other direction" $ do
      let fc = Origami.foldMany [((1, -1), (0, 0))] circle
      it "one layer on the line" $ do
         fc (0, 0) `shouldBe` 1
         fc (-5, 5) `shouldBe` 1
         fc (5, -5) `shouldBe` 1
      it "two layers on the left side" $ do
        fc (0, -0.5) `shouldBe` 2
        fc (0, -10) `shouldBe` 2
        fc (-10, 0) `shouldBe` 2
      it "zero layers of the right side" $ do
        fc (0, 0.5) `shouldBe` 0
        fc (0, 10) `shouldBe` 0
        fc (10, 0) `shouldBe` 0

    describe "folding twice - y = 0 and x = 0" $ do
      let fc = Origami.foldMany [((0, 0), (1, 0)), ((0, 1), (0, 0))] circle -- upper right quarter of the circle
      it "one layer on both lines" $ do
        fc (0, 0) `shouldBe` 1
        fc (0, 5) `shouldBe` 1
        fc (5, 0) `shouldBe` 1
        fc (0, 10) `shouldBe` 1
        fc (10, 0) `shouldBe` 1
      it "two layers in the upper right quarter" $ do
        fc (1, 1) `shouldBe` 2
        fc (0.5, 8) `shouldBe` 2
        fc (9, 0.2) `shouldBe` 2
        fc (5, 7) `shouldBe` 2
      it "zero layers in other quarters" $ do
        fc (-1, 0) `shouldBe` 0
        fc (-1, 1) `shouldBe` 0
        fc (-1, -1) `shouldBe` 0
        fc (0, -1) `shouldBe` 0
        fc (0, -10) `shouldBe` 0
        fc (-1, -1) `shouldBe` 0

  describe "Origami - square folding" $ do
    let square = Origami.rectangle (-5, -5) (5, 5)
    describe "without folding" $ do
      it "one layer in square" $ do
        square (0, 0) `shouldBe` 1
        square (5, 5) `shouldBe` 1
        square (5, -5) `shouldBe` 1
        square (-5, -5) `shouldBe` 1
        square (-5, 5) `shouldBe` 1
      it "zero layers outside square" $ do
        square (5, 6) `shouldBe` 0
        square (6, 5) `shouldBe` 0
        square (-6, -5) `shouldBe` 0
        square (-5, -6) `shouldBe` 0

    describe "folding once - vertical line" $ do
      let fs = Origami.foldMany [((0, 0), (0, 1))] square
      it "one layer on the line" $ do
         fs (0, 0) `shouldBe` 1
         fs (0, 5) `shouldBe` 1
         fs (0, -5) `shouldBe` 1
      it "two layers on the left side" $ do
         fs (-5, -5) `shouldBe` 2
         fs (-5, 5) `shouldBe` 2
         fs (-0.1, 0) `shouldBe` 2
         fs (-2, 3) `shouldBe` 2
      it "zero layers of the right side" $ do
         fs (3, 2) `shouldBe` 0
         fs (5, 0) `shouldBe` 0
      it "zero layers on the line outside square" $ do
         fs (0, 6) `shouldBe` 0
         fs (0, -6) `shouldBe` 0

    describe "folding once - horizontal line" $ do
      let fs = Origami.foldMany [((0, 0), (1, 0))] square
      it "one layer on the line" $ do
         fs (0, 0) `shouldBe` 1
         fs (-5, 0) `shouldBe` 1
         fs (5, 0) `shouldBe` 1
      it "two layers on the left side" $ do
         fs (0, 5) `shouldBe` 2
         fs (-5, 2) `shouldBe` 2
         fs (5, 2) `shouldBe` 2
      it "zero layers of the right side" $ do
         fs (-5, -2) `shouldBe` 0
         fs (5, -2) `shouldBe` 0
         fs (0, -5) `shouldBe` 0
      it "zero layers on the line outside square" $ do
         fs (-6, 0) `shouldBe` 0
         fs (6, 0) `shouldBe` 0

    describe "folding once - f(x) = x" $ do
      let fs = Origami.foldMany [((0, 0), (1, 1))] square
      it "one layer on the line" $ do
         fs (0, 0) `shouldBe` 1
         fs (-5, -5) `shouldBe` 1
         fs (5, 5) `shouldBe` 1
      it "two layers on the left side" $ do
         fs (0, 0.5) `shouldBe` 2
         fs (0, 5) `shouldBe` 2
         fs (-5, -4) `shouldBe` 2
      it "zero layers of the right side" $ do
         fs (0, -0.5) `shouldBe` 0
         fs (0, -5) `shouldBe` 0
         fs (5, 4) `shouldBe` 0

    describe "folding once - f(x) = -x" $ do
      let fs = Origami.foldMany [((0, 0), (1, -1))] square
      it "one layer on the line" $ do
         fs (0, 0) `shouldBe` 1
         fs (-5, 5) `shouldBe` 1
         fs (5, -5) `shouldBe` 1
      it "two layers on the left side" $ do
         fs (0, 0.5) `shouldBe` 2
         fs (0, 5) `shouldBe` 2
         fs (4, 3) `shouldBe` 2
      it "zero layers of the right side" $ do
         fs (0, -0.5) `shouldBe` 0
         fs (0, -5) `shouldBe` 0
         fs (-4, 0) `shouldBe` 0
      it "zero layers on the line outside of rectangle" $ do
         fs (0, 0) `shouldBe` 1
         fs (-6, 6) `shouldBe` 1
         fs (6, -6) `shouldBe` 1

    describe "folding twice - y = 0 and x = 0" $ do
      let fs = Origami.foldMany [((0, 0), (1, 0)), ((0, 1), (0, 0))] square -- upper right quarter of the square
      it "one layer on both lines" $ do
        fs (0, 0) `shouldBe` 1
        fs (0, 5) `shouldBe` 1
        fs (5, 0) `shouldBe` 1
        fs (0, -5) `shouldBe` 1
        fs (-5, 0) `shouldBe` 1
      it "two layers in the upper right quarter" $ do
        fs (1, 1) `shouldBe` 2
        fs (0.5, 3) `shouldBe` 2
        fs (5, 0.2) `shouldBe` 2
        fs (0, 2) `shouldBe` 2
      it "zero layers in other quarters" $ do
        fs (-1, 0) `shouldBe` 0
        fs (-1, 1) `shouldBe` 0
        fs (-1, -1) `shouldBe` 0
        fs (0, -1) `shouldBe` 0
        fs (0, -10) `shouldBe` 0
        fs (-1, -1) `shouldBe` 0
        fs (-5, -5) `shouldBe` 0
      it "zero layers on both lines outside of rectangle" $ do
        fs (0, 6) `shouldBe` 0
        fs (5, 0) `shouldBe` 0
        fs (0, -5.1) `shouldBe` 0
        fs (-5.1, 0) `shouldBe` 0


