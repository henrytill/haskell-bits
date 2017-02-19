-- |
-- Module      : Zipper
-- Description : Huet's Zipper
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- <http://gallium.inria.fr/~huet/PUBLIC/zip.pdf Functional Pearls: The Zipper>, by Gerard Huet
--
module Zipper
  ( -- * A trie...
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

-- * A trie...

data Tree a
  = Item a
  | Section [Tree a]
  deriving (Eq, Show)

-- * ...and its Zipper

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

-- * Navigation


-- |
-- >>> goRight $ goDown example
-- Loc (Item "+") (Node [Section [Item "a",Item "*",Item "b"]] Top [Section [Item "c",Item "*",Item "d"]])
-- >>> goRight $ goRight $ goDown example
-- Loc (Section [Item "c",Item "*",Item "d"]) (Node [Item "+",Section [Item "a",Item "*",Item "b"]] Top [])
--
goRight (Loc t p) = case p of
  Top                    -> error "right of top"
  Node left up (r:right) -> Loc r (Node (t:left) up right)
  _                      -> error "right of last"

-- |
-- >>> goLeft (goRight (goDown example)) == goDown example
-- True
--
goLeft (Loc t p) = case p of
  Top                    -> error "left of top"
  Node (l:left) up right -> Loc l (Node left up (t:right))
  Node [] up right       -> error "left of first"

-- |
-- >>> goDown example
-- Loc (Section [Item "a",Item "*",Item "b"]) (Node [] Top [Item "+",Section [Item "c",Item "*",Item "d"]])
-- >>> goDown $ goDown example
-- Loc (Item "a") (Node [] (Node [] Top [Item "+",Section [Item "c",Item "*",Item "d"]]) [Item "*",Item "b"])
--
goDown (Loc t p) = case t of
  Item _             -> error "down of item"
  Section (t1:trees) -> Loc t1 (Node [] p trees)
  _                  -> error "down of empty"

-- |
-- >>> goUp (goDown example) == example
-- True
--
goUp (Loc t p) = case p of
  Top                -> error "up of top"
  Node left up right -> Loc (Section (reverse left ++ (t:right))) up

-- | A function to access the /n/th son of the current tree
nthLoc :: (Ord t, Num t) => Location a -> t -> Location a
nthLoc loc = nthRec
  where
    nthRec 1 = goDown loc
    nthRec n = if n > 0
               then goRight $ nthRec (n - 1)
               else error "nthLoc expects a positive integer"
