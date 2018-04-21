module WorkerWrapper where

import           Prelude hiding (abs)


fix :: (a -> a) -> a
fix f = f (fix f)

type H a = [a] -> [a]

rep :: [a] -> H a
rep xs = (xs ++)

abs :: H a -> [a]
abs f = f []

rev :: [a] -> [a]
rev = fix body

body :: ([a] -> [a]) -> ([a] -> [a])
body f []     = []
body f (x:xs) = f xs ++ [x]

unwrap :: ([a] -> [a]) -> ([a] -> H a)
unwrap f = rep . f

wrap :: ([a] -> H a) -> ([a] -> [a])
wrap g = abs . g

work :: [a] -> H a
work []     = rep []
work (x:xs) = rep (wrap work xs ++ [x])

rev' :: [a] -> [a]
rev' xs = work xs []
