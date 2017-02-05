module TwoLevel where

data RTree = RTip
           | RLeaf Int
           | RFork RTree RTree
           deriving Show

data T x = Tip
         | Leaf Int
         | Fork x x
         deriving Show

data Tree = Wrap (T Tree) deriving Show

unwrap :: Tree -> T Tree
unwrap (Wrap x) = x

tip :: Tree
tip = Wrap Tip

leaf :: Int -> Tree
leaf x = Wrap (Leaf x)

fork :: Tree -> Tree -> Tree
fork x y = Wrap (Fork x y)

convert :: Tree -> RTree
convert t = case unwrap t of
  Tip      -> RTip
  Leaf x   -> RLeaf x
  Fork l r -> RFork (convert l) (convert r)

example :: Tree
example = fork (fork (leaf 1) (leaf 2)) (fork (leaf 3) tip)

rExample :: RTree
rExample = convert example
