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
      isEmpty (T.singleton 0) `shouldBe` False

    it "adding single element to empty tree" $ do
      let foo = 0
      add 0 emptyTree `shouldBe` (T.singleton 0)

    it "adding minimal element to a tree, then retrieving it" $ do
      let originalTree = (T.singleton 10) :: T.LeftWingTree Int
      let tree = add 0 originalTree
      deleteMin tree `shouldBe` (Just 0, originalTree)

    it "adding non-minimal element to a tree, then retrieving minimum" $ do
      let tree = add 10 (T.singleton 0) :: T.LeftWingTree Int
      deleteMin tree `shouldBe` (Just 0, (T.singleton 10))

    it "joining a tree with empty tree returns the tree" $ do
      let tree = (T.singleton 0) in join emptyTree tree `shouldBe` tree
      let tree = (T.singleton 0) in join tree emptyTree `shouldBe` tree

    it "joining two trees - order of trees does not matter" $ do
      let tree1 = add 1 (T.singleton 0) :: T.LeftWingTree Int
      let tree2 = add 4 (T.singleton 5) :: T.LeftWingTree Int
      join tree1 tree2 `shouldBe` join tree2 tree1

    it "prioritySort sorts list of integers" $ do
      T.prioritySort []           `shouldBe` []
      T.prioritySort [2, 0, 1, 1] `shouldBe` [0, 1, 1, 2]
      T.prioritySort [-1, 1, 0]   `shouldBe` [-1, 0, 1]
      T.prioritySort [4, 1, 3, 2] `shouldBe` [1, 2, 3, 4]


emptyTree :: T.LeftWingTree Int
emptyTree = T.EmptyTree
