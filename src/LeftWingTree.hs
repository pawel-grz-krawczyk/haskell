module LeftWingTree
    ( LeftWingTree(EmptyTree, Node)
    ) where

import PriorityQueue

data LeftWingTree a = EmptyTree |
                    Node {
                      v :: a,
                      h :: Int,
                      left :: LeftWingTree a,
                      right :: LeftWingTree a
                    } deriving (Eq, Read, Show)


instance PriorityQueue LeftWingTree where
  empty = undefined

  isEmpty EmptyTree = undefined

  add e tree = undefined

  deleteMin tree = undefined

  join t1 t2 = undefined
