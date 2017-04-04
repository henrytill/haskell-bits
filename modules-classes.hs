{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module ModulesAsClasses where

class SetSig a where
  type Elem a
  type Set a
  empty  :: a -> Set a
  member :: a -> Elem a -> Set a -> Bool
  insert :: a -> Elem a -> Set a -> Set a

data IntSet = IntSet

instance SetSig IntSet where
  type Elem IntSet = Int
  type Set IntSet  = [Int]
  empty _          = []
  member _         = elem
  insert _ i s     = if member IntSet i s
                     then s
                     else i : s

data IntSet' = IntSet'

instance SetSig IntSet' where
  type Elem IntSet' = Elem IntSet
  type Set IntSet'  = Set IntSet
  empty _           = empty IntSet
  member _          = member IntSet
  insert _          = insert IntSet
