{-# LANGUAGE RankNTypes, GADTs #-}

module Double where

double2 :: (forall a. [a] -> [a] -> [a]) -> [b] -> [c] -> ([b], [c])
double2 f l r = (f l l, f r r)

data Perfect a where
    ZeroP :: a -> Perfect a
    SuccP :: Perfect (a, a) -> Perfect a

depthP :: Perfect a -> Int
depthP (ZeroP _) = 0
depthP (SuccP p) = succ (depthP p)
