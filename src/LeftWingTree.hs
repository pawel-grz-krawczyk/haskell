module LeftWingTree(
      LeftWingTree(EmptyTree, Node),
      singleton,
      prioritySort
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


singleton :: a -> LeftWingTree a
singleton a = Node { v = a, h = 0, left = EmptyTree, right = EmptyTree }

prioritySort :: [Int] -> [Int]
prioritySort xs = sort tree []
  where
    nodes = map singleton xs
    tree = foldl join EmptyTree nodes
    sort EmptyTree acc = reverse acc
    sort tree acc =  e:acc where
      (Just e, tree') = deleteMin tree
