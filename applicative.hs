module ApplicativeExamples where

-- Transposing 'matrices'

transpose :: [[a]] -> [[a]]
transpose []       = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

-- λ> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

-- The binary zipWith is one of a family of operations that 'vectorise' pure
-- functions.

-- repeat :: a -> [a]
-- repeat x = x : repeat x

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _      _      = []

transpose' :: [[a]] -> [[a]]
transpose' []       = repeat []
transpose' (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss
