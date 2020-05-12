module PriorityQueue(
       PriorityQueue(empty, isEmpty, add, deleteMin, join),
       Priority
   ) where

class PriorityQueue q where
  empty     :: q a
  isEmpty   :: q a -> Bool
  add       :: (Priority a) => a -> q a -> q a -- is it possible to move this constraint somehwere else?
  deleteMin :: (Priority a) => q a -> (Maybe a, q a)
  join      :: (Priority a) => q a -> q a -> q a

class Priority a where
  priority :: a -> Int

instance Priority Int where
  priority = id
  