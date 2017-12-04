-- |
-- Module      : STUnionFind
-- Description : An implementation of the Union Find algorithm
--
-- <http://kseo.github.io/posts/2014-01-30-implementing-union-find-in-haskell.html>
--
module STUnionFind where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

data UnionFind s = UnionFind
  { ids :: STUArray s Int Int
  , szs :: STUArray s Int Int
  }

newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n =
  liftM2 UnionFind (newListArray (0, n - 1) [0..n - 1]) (newArray (0, n - 1) 1)

find :: UnionFind s -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: UnionFind s -> Int -> ST s Int
root uf i = do
  x <- readArray (ids uf) i
  if x /= i
    then do
    gpid <- readArray (ids uf) x
    writeArray (ids uf) i gpid
    root uf x
    else return i

unite :: UnionFind s -> Int -> Int -> ST s ()
unite uf p q = do
  i   <- root uf p
  j   <- root uf q
  szi <- readArray (szs uf) i
  szj <- readArray (szs uf) j
  if szi < szj
    then do writeArray (ids uf) i j
            writeArray (szs uf) j (szi + szj)
    else do writeArray (ids uf) j i
            writeArray (szs uf) i (szj + szi)

-- | Do it
--
-- Examples:
--
-- >>> example
-- True
example :: IO ()
example = print $ runST $ do
  uf <- newUnionFind 10
  unite uf 3 4 -- 0, 1, 2, {3, 4}, 5, 6, 7, 8, 9
  unite uf 4 9 -- 0, 1, 2, {3, 4, 9}, 5, 6, 7, 8
  unite uf 8 0 -- {0, 8}, 1, 2, {3, 4, 9}, 5, 6, 7, 8
  unite uf 2 3 -- {0, 8}, 1, {2, 3, 4, 9}, 5, 6, 7
  unite uf 5 6 -- {0, 8}, 1, {2, 3, 4, 9}, {5, 6}, 7
  unite uf 5 9 -- {0, 8}, 1, {2, 3, 4, 5, 6, 9}, 7
  unite uf 7 3 -- {0, 8}, 1, {2, 3, 4, 5, 6, 7, 9}
  unite uf 4 8 -- 1, {0, 2, 3, 4, 5, 6, 7, 8, 9}
  find uf 0 9 -- False
