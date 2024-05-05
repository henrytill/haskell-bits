{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : TwoLevel
-- Description : Two-level types
module TwoLevel where

newtype Fix f = In {out :: f (Fix f)}

deriving instance (Eq (f (Fix f))) => Eq (Fix f)

deriving instance (Ord (f (Fix f))) => Ord (Fix f)

deriving instance (Show (f (Fix f))) => Show (Fix f)

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

-- * A tree

data T x
  = Tip
  | Leaf Int
  | Fork x x
  deriving (Eq, Show, Functor)

type Tree = Fix T

tip :: Tree
tip = In Tip

leaf :: Int -> Tree
leaf x = In (Leaf x)

fork :: Tree -> Tree -> Tree
fork x y = In (Fork x y)

example :: Tree
example = fork (fork (leaf 1) (leaf 2)) (fork (leaf 3) tip)

data RTree
  = RTip
  | RLeaf Int
  | RFork RTree RTree
  deriving (Show)

convertAlg :: Algebra T RTree
convertAlg Tip = RTip
convertAlg (Leaf x) = RLeaf x
convertAlg (Fork l r) = RFork l r

rExample :: RTree
rExample = cata convertAlg example
