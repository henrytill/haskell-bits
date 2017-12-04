module Misc where

import           Data.List (unfoldr)


chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr f
  where
    f [] = Nothing
    f xs = Just (take n xs, drop n xs)

-- | Quicksort (after Hudak)
qs :: Ord t => [t] -> [t]
qs []     = []
qs (x:xs) = qs [y | y <- xs, y <  x ] ++ [x] ++
            qs [y | y <- xs, y >= x ]


-- | Sieve of... (Erasothenes ?)
sieve :: [Int] -> [Int]
sieve []       = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes :: [Int]
primes = sieve [2..]
