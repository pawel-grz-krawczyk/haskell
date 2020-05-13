{-# LANGUAGE TypeOperators #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified LeftWingTree as T
import PriorityQueue

data Foo a = A | B a deriving(Eq, Show)

main :: IO ()
main = hspec $ do
  describe "LeftWingTree priority queue" $ do
    it "empty returns empty tree" $ do
      empty `shouldBe` emptyTree

    it "isEmpty returns true given empty tree" $ do
      isEmpty emptyTree `shouldBe` True

    it "isEmpty returns false given non-empty tree" $ do
      isEmpty (T.singletonQ 0) `shouldBe` False

    it "adding single element to empty tree" $ do
      let foo = 0
      add 0 emptyTree `shouldBe` (T.singletonQ 0)

    it "deleting min of single node tree" $ do
      deleteMin (T.singletonQ 0) `shouldBe` (Just 0, emptyTree)

    it "adding minimal element to a tree, then retrieving it" $ do
      let originalTree = (T.singletonQ 10) :: T.LeftWingTree Int
      let tree = add 0 originalTree
      deleteMin tree `shouldBe` (Just 0, originalTree)

    it "adding non-minimal element to a tree, then retrieving minimum" $ do
      let tree = add 10 (T.singletonQ 0) :: T.LeftWingTree Int
      deleteMin tree `shouldBe` (Just 0, (T.singletonQ 10))

    it "joining a tree with empty tree returns the tree" $ do
      let tree = (T.singletonQ 0) in join emptyTree tree `shouldBe` tree
      let tree = (T.singletonQ 0) in join tree emptyTree `shouldBe` tree

    it "joining two trees - order of trees does not matter" $ do
      let tree1 = add 1 (T.singletonQ 0) :: T.LeftWingTree Int
      let tree2 = add 4 (T.singletonQ 5) :: T.LeftWingTree Int
      join tree1 tree2 `shouldBe` join tree2 tree1

  describe "Priority sorting" $ do
    it "empty list" $ do
      T.prioritySort ([] :: [Int])  `shouldBe` ([] :: [Int])

    it "duplicate priorities present" $ do
      T.prioritySort ([2, 0, 1, 1] :: [Int]) `shouldBe` ([0, 1, 1, 2]:: [Int])

    it "both positive and negative priorities" $ do
      T.prioritySort ([-1, 1, 0] :: [Int])   `shouldBe` ([-1, 0, 1] :: [Int])

    it "positive priorities" $ do
      T.prioritySort ([4, 1, 3, 2] :: [Int]) `shouldBe` ([1, 2, 3, 4] :: [Int])

    it "positive priorities" $ do
      T.prioritySort ([-4, -1, -3, -2] :: [Int]) `shouldBe` ([-4, -3, -2, -1] :: [Int])


emptyTree :: T.LeftWingTree Int
emptyTree = T.emptyQ
