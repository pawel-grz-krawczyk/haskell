{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LeftWingTree(
      LeftWingTree(Node, v, h, left, right), -- would it be possible to export Node just in tests?
      singletonQ,
      emptyQ,
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


instance (Priority a) => PriorityQueue LeftWingTree a where
  empty = EmptyTree

  isEmpty EmptyTree = True
  isEmpty _ = False

  add e tree = join tree $ singletonQ e

  deleteMin EmptyTree = (Nothing, EmptyTree)
  deleteMin Node {v=v, h=_, left=left, right=right} = (Just v, join left right)

  join EmptyTree tree = tree
  join tree EmptyTree = tree
  join t1@Node {v=v1} t2@Node {v=v2}
    | priority v1 < priority v2 =  join' t1 t2
    | otherwise = join' t2 t1


join' :: (Priority a) => LeftWingTree a -> LeftWingTree a -> LeftWingTree a
join' Node {v = v1, left = left1, right = right1} smallerPriorityTree =
  Node {v = v1, h = height rightTree + 1, left = leftTree, right = rightTree}
  where
    t3 = join right1 smallerPriorityTree
    (leftTree, rightTree) =
      if height t3 > height left1
        then (t3, left1)
        else (left1, t3)

height :: LeftWingTree a -> Int
height EmptyTree = -1
height tree = h tree

singletonQ :: a -> LeftWingTree a
singletonQ a = Node { v = a, h = 0, left = EmptyTree, right = EmptyTree }

emptyQ:: LeftWingTree a
emptyQ = EmptyTree

prioritySort :: (Priority a) => [a] -> [a]
prioritySort xs = sort tree []
  where
    tree = foldl join EmptyTree . map singletonQ $ xs
    sort :: (Priority a) => LeftWingTree a -> [a] -> [a]
    sort EmptyTree acc = reverse acc
    sort tree acc =  sort tree' (e : acc) where
      (Just e, tree') = deleteMin tree
