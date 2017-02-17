{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}

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

newtype AbsSet a = AbsSetImpl [a]

instance SetSig IntSet' where
  type Elem IntSet'              = Int
  type Set IntSet'               = AbsSet Int
  empty _                        = AbsSetImpl []
  member _ i (AbsSetImpl s)      = i `elem` s
  insert _ i s @ (AbsSetImpl xs) = if member IntSet' i s
                                   then s
                                   else AbsSetImpl (i : xs)
