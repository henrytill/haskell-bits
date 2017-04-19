-- |
-- Module      : Zipper
-- Description : Huet's Zipper
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- <http://gallium.inria.fr/~huet/PUBLIC/zip.pdf Functional Pearls: The Zipper>, by Gerard Huet
--
module Zipper
  ( -- * A tree...
    Tree(..)
    -- * ...and its Zipper
  , Path(..)
  , Location(..)
    -- * Navigation
  , goRight
  , goLeft
  , goDown
  , goUp
  , nthLoc
  ) where

-- $setup
-- >>> let tree = Section [Section [Item "a", Item "*", Item "b"], Item "+", Section [Item "c", Item "*", Item "d"]] :: Tree String
-- >>> let example = Loc tree Top :: Location String

-- | A 'Tree' is the data structure for which we implement a Zipper.
--
data Tree a
  = Item a
  | Section [Tree a]
  deriving (Eq, Show)

-- | A 'Path' is like a Zipper, allowing one to rip the tree structure down to a
-- certain location.
--
data Path a
  = Top
  | Node [Tree a] (Path a) [Tree a]
  deriving (Eq, Show)

-- | A 'Location' in the 'Tree' addresses a subtree, together with its 'Path'.
--
-- It consists of a distinguished 'Tree', the current focus of
-- attention, and its 'Path', representing the surrounding context.
--
-- >>> let tree = Section [Section [Item "a", Item "*", Item "b"], Item "+", Section [Item "c", Item "*", Item "d"]] :: Tree String
-- >>> let example = Loc tree Top :: Location String
-- >>> example
-- Loc (Section [Section [Item "a",Item "*",Item "b"],Item "+",Section [Item "c",Item "*",Item "d"]]) Top
--
data Location a = Loc (Tree a) (Path a) deriving (Eq, Show)

-- |
-- >>> goRight $ goDown example
-- Loc (Item "+") (Node [Section [Item "a",Item "*",Item "b"]] Top [Section [Item "c",Item "*",Item "d"]])
-- >>> goRight $ goRight $ goDown example
-- Loc (Section [Item "c",Item "*",Item "d"]) (Node [Item "+",Section [Item "a",Item "*",Item "b"]] Top [])
--
goRight :: Location a -> Location a
goRight (Loc t p) = case p of
  Top                    -> error "right of top"
  Node left up (r:right) -> Loc r (Node (t:left) up right)
  _                      -> error "right of last"

-- |
-- >>> goLeft (goRight (goDown example)) == goDown example
-- True
--
goLeft :: Location a -> Location a
goLeft (Loc t p) = case p of
  Top                    -> error "left of top"
  Node (l:left) up right -> Loc l (Node left up (t:right))
  Node [] up right       -> error "left of first"

-- |
-- >>> goDown example
-- Loc (Section [Item "a",Item "*",Item "b"]) (Node [] Top [Item "+",Section [Item "c",Item "*",Item "d"]])
-- >>> goDown $ goDown example
-- Loc (Item "a") (Node [] (Node [] Top [Item "+",Section [Item "c",Item "*",Item "d"]]) [Item "*",Item "b"])
-- >>> goRight $ goDown $ goDown example
-- Loc (Item "*") (Node [Item "a"] (Node [] Top [Item "+",Section [Item "c",Item "*",Item "d"]]) [Item "b"])
--
goDown :: Location a -> Location a
goDown (Loc t p) = case t of
  Item _             -> error "down of item"
  Section (t1:trees) -> Loc t1 (Node [] p trees)
  _                  -> error "down of empty"

-- |
-- >>> goUp (goDown example) == example
-- True
--
goUp :: Location a -> Location a
goUp (Loc t p) = case p of
  Top                -> error "up of top"
  Node left up right -> Loc (Section (reverse left ++ (t:right))) up

-- | A function to access the /n/th son of the current tree
--
-- >>> nthLoc example 1
-- Loc (Section [Item "a",Item "*",Item "b"]) (Node [] Top [Item "+",Section [Item "c",Item "*",Item "d"]])
-- >>> nthLoc example 3
-- Loc (Section [Item "c",Item "*",Item "d"]) (Node [Item "+",Section [Item "a",Item "*",Item "b"]] Top [])
--
nthLoc :: (Ord t, Num t) => Location a -> t -> Location a
nthLoc loc = nthRec
  where
    nthRec 1         = goDown loc
    nthRec n | n > 0 = goRight $ nthRec (n - 1)
    nthRec _         = error "nthLoc expects a positive integer"
