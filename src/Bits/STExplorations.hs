{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      : Bits.STExplorations
-- Description : Understanding ST
module Bits.STExplorations where

import Control.Monad.ST
import Data.STRef

swap :: STRef s a -> STRef s a -> ST s ()
swap v w = do
  a <- readSTRef v
  b <- readSTRef w
  writeSTRef v b
  writeSTRef w a

demo :: ST s (STRef s Integer)
demo = do
  r <- newSTRef 23
  s <- newSTRef 42
  swap r s
  return r

runThis :: ST s Integer
runThis = do
  r <- demo
  readSTRef r

v :: Integer
v = runST runThis

-- But you can't do the following:
{-
x = runST demo
y = runST (newSTRef True >>= return)
z = runST (readSTRef y >>= return)
-}
