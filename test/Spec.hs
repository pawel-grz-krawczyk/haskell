{-# LANGUAGE TypeOperators #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified LeftWingTree as T
import PriorityQueue

data Foo a = A | B a deriving(Eq, Show)

main :: IO ()
main = hspec $ do
  describe "LeftWingTree" $ do
    it "empty returns empty tree" $ do
      empty `shouldBe` emptyTree

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

emptyTree :: T.LeftWingTree Int
emptyTree = T.EmptyTree