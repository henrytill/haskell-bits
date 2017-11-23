module Classics where

-- Quicksort in Haskell (after Hudak)

qs []     = []
qs (x:xs) = qs [y | y <- xs, y <  x ] ++ [x] ++
            qs [y | y <- xs, y >= x ]


-- Sieve of Erasothenes (?)

sieve :: [Int] -> [Int]
sieve []       = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes :: [Int]
primes = sieve [2..]
