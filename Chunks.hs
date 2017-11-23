module Chunks where

import Data.List (unfoldr)

chunks n = unfoldr f
  where
    f [] = Nothing
    f xs = Just (take n xs, drop n xs)
