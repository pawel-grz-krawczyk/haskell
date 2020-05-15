module LeftWingTreeSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import LeftWingTree
import PriorityQueue

type Queue = LeftWingTree Int

spec :: Spec
spec =  do
  describe "LeftWingTree priority queue" $ do
    it "empty returns empty tree" $ do
      empty `shouldBe` emptyQueue

    it "isEmpty returns true given empty tree" $ do
      isEmpty emptyQueue `shouldBe` True

    it "isEmpty returns false given non-empty tree" $ do
      isEmpty ((singletonQ 0) :: Queue) `shouldBe` False

    it "adding single element to empty tree" $ do
      add 0 emptyQueue `shouldBe` (singletonQ 0)

    it "deleting min of single node tree" $ do
      deleteMin (singletonQ 0) `shouldBe` (Just 0, emptyQueue)

    it "adding minimal element to a tree, then retrieving it" $ do
      let originalTree = singletonQ 10 :: Queue
      let tree = add 0 originalTree
      deleteMin tree `shouldBe` (Just 0, originalTree)

    it "adding non-minimal element to a tree, then retrieving minimum" $ do
      let tree = add 10 (singletonQ 0) :: Queue
      deleteMin tree `shouldBe` (Just 0, singletonQ 10)

    it "joining a tree with empty tree returns the tree" $ do
      let tree = singletonQ 0 in join emptyQueue tree `shouldBe` tree
      let tree = singletonQ 0 in join tree emptyQueue `shouldBe` tree

    it "joining two trees - order of trees does not matter" $ do
      let tree1 = add 1 (singletonQ 0) :: Queue
      let tree2 = add 4 (singletonQ 5) :: Queue
      join tree1 tree2 `shouldBe` join tree2 tree1

    it "joining two complex trees" $ do
      let tree1 = Node { v = 2, h = 1, left = singletonQ 10, right = singletonQ 6 }
      let tree2 = Node { v = 4, h = 0 , right = emptyQueue,
                           left = Node { v = 6, h = 0 , right = emptyQueue,
                                             left = singletonQ 8
                           }
      }
      let expectedJoinResult = Node {
         v = 2,
         h = 1,
         right = singletonQ 10,
         left = Node {
            v = 4,
            h = 1,
            right = singletonQ 6,
            left = Node {
              v = 6,
              h = 0,
              right = emptyQueue,
              left = singletonQ 8
            }
         }
      }
      join tree1 tree2 `shouldBe` join tree2 tree1
      join tree1 tree2 `shouldBe` expectedJoinResult

  describe "Priority sorting" $ do
    it "empty list" $ do
      prioritySort ([] :: [Int])  `shouldBe` ([] :: [Int])

    it "duplicate priorities present" $ do
      prioritySort ([2, 0, 1, 1] :: [Int]) `shouldBe` ([0, 1, 1, 2]:: [Int])

    it "both positive and negative priorities" $ do
      prioritySort ([-1, 1, 0] :: [Int])   `shouldBe` ([-1, 0, 1] :: [Int])

    it "positive priorities" $ do
      prioritySort ([4, 1, 3, 2] :: [Int]) `shouldBe` ([1, 2, 3, 4] :: [Int])

    it "positive priorities" $ do
      prioritySort ([-4, -1, -3, -2] :: [Int]) `shouldBe` ([-4, -3, -2, -1] :: [Int])


emptyQueue :: Queue
emptyQueue = emptyQ
