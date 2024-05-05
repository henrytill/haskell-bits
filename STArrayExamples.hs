-- |
-- Module      : STArrayExamples
-- Description : Some STArray examples
--
-- Code from John Launchbury and Simon Peyton-Jones'
-- /Lazy Functional State Threads/
module STArrayExamples where

import Control.Monad.ST
import Data.Array
import Data.Array.ST

accumArray :: (Ix i) => (a -> b -> a) -> a -> (i, i) -> [(i, b)] -> ST s (STArray s i a)
accumArray f z bnds ivs = do
  marr <- newArray bnds z
  fill marr f ivs
  return marr

fill :: (MArray a t m, Ix i) => a i t -> (t -> t1 -> t) -> [(i, t1)] -> m ()
fill _ _ [] = return ()
fill a f ((i, v) : ivs) = do
  x <- readArray a i
  writeArray a i (f x v)
  fill a f ivs

accumArray' :: (Ix i) => (a -> b -> a) -> a -> (i, i) -> [(i, b)] -> ST s (STArray s i a)
accumArray' f z bnds ivs = do
  marr <- newArray bnds z
  mapM_ (update marr f) ivs
  return marr

update :: (MArray a t m, Ix i) => a i t -> (t -> t1 -> t) -> (i, t1) -> m ()
update a f (i, v) = do
  x <- readArray a i
  writeArray a i (f x v)

-- Examples

hist :: (Ix i) => (i, i) -> [i] -> ST s (STArray s i Int)
hist bnds is = accumArray' (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]

binSort :: (Ix i) => (i, i) -> (a -> i) -> [a] -> ST s (STArray s i [a])
binSort bnds key vs = accumArray' (flip (:)) [] bnds [(key v, v) | v <- vs]
