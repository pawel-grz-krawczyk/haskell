{-# LANGUAGE MultiParamTypeClasses #-}
module PriorityQueue(
       PriorityQueue(empty, isEmpty, add, deleteMin, join),
       Priority(priority)
   ) where

class (Priority a) => PriorityQueue q a where
  empty     :: q a
  isEmpty   :: q a -> Bool
  add       :: a -> q a -> q a
  deleteMin :: q a -> (Maybe a, q a)
  join      :: q a -> q a -> q a

class Priority a where
  priority :: a -> Int

instance Priority Int where
  priority = id
