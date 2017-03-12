{-# OPTIONS_GHC -Wall #-}

module Origami where

-- Unfolds generate data structures and folds consume them

-- * Origami with lists: sorting

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- $setup
-- >>> let x = Cons 1 (Cons 2 (Cons 3 Nil))
-- >>> let y = Cons 4 (Cons 5 (Cons 6 Nil))

-- | A conctructor for singleton `List`s
--
-- >>> wrap 7
-- Cons 7 Nil
--
wrap :: a -> List a
wrap x = Cons x Nil

-- | Detects empty lists
--
-- >>> nil Nil
-- True
-- >>> nil (Cons 1 Nil)
-- False
--
nil :: List a -> Bool
nil Nil        = True
nil (Cons _ _) = False

-- ** Folds for lists

-- | A natural fold for lists.  This is equivalent to `foldr`
--
-- >>> foldL (+) 0 x
-- 6
--
foldL :: (a -> b -> b) -> b -> List a -> b
foldL _ e Nil         = e
foldL f e (Cons x xs) = f x (foldL f e xs)

-- *** Exercise 3.2

-- |
-- >>> mapL (+1) x
-- Cons 2 (Cons 3 (Cons 4 Nil))
--
mapL :: (a -> b) -> List a -> List b
mapL f = foldL (\a acc -> Cons (f a) acc) Nil

-- |
-- >>> appendL x y
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
appendL :: List a -> List a -> List a
appendL xs ys = foldL Cons ys xs

-- |
-- >>> let xy = Cons x (Cons y Nil)
-- >>> concatL xy
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
concatL :: List (List a) -> List a
concatL = foldL appendL Nil

-- *** Exercise 3.3

-- $
-- >>> foldL (+) 0 . mapL (*5) $ x
-- 30
-- >>> foldL ((+) . (*5)) 0 $ x
-- 30

-- | A classic application of of `foldL` - the insertion sort algorithm.
--
-- IPFH defines `insert` using `takeWhile` and `dropWhile`, but we make the
-- recursion pattern explicit so that we cant study it.
--
-- >>> isort (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
isort :: Ord a => List a -> List a
isort = foldL insert Nil
  where
    insert :: Ord a => a -> List a -> List a
    insert y Nil         = wrap y
    insert y (Cons x xs)
      | y < x            = Cons y (Cons x xs)
      | otherwise        = Cons x (insert y xs)

-- *** Exercise 3.4

-- |
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insert1 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
--
insert1 :: Ord a => a -> List a -> List a
insert1 y = snd . foldL inserter (Nil, wrap y)
  where
    inserter x (xs, acc)
      | y < x            = (Cons x xs, Cons y (Cons x xs))
      | otherwise        = (Cons x xs, Cons x acc)

-- |
-- >>> isort1 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
isort1 :: Ord a => List a -> List a
isort1 = foldL insert1 Nil

-- *** Exercise 3.5

-- $paramorphism
--
-- A paramorphism captures a recursion pattern where the result depends not
-- only on a recursive call on a substructre, but also on the substructure
-- itself.

-- | The paramorphism operator for a list.  The argument `f` takes a copy of the
-- tail `xs` along with the result `paraL f e xs` of the recursive call on that
-- tail.
paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL _ e Nil         = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)

-- |
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insert2 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
--
insert2 :: Ord a => a -> List a -> List a
insert2 y = paraL f (wrap y)
  where
    f x (xs, acc)
      | y < x     = Cons y (Cons x xs)
      | otherwise = Cons x acc

-- ** Unfolds for lists

-- $
-- The dual of folding is unfolding

-- | An implementation of `unfoldr` for our `List` datatype
unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
                 Nothing     -> Nil
                 Just (x, v) -> Cons x (unfoldL' f v)

-- $
-- Sometimes it is convenient to provide the single argument of `unfoldL'` as
-- three components: a predicate indicating when that argument should return
-- `Nothing`, and two functions yielding the two components of the pair when it
-- does not.

-- | `unfoldL` takes a predicate `p` indicating when the seed should unfold the
-- empty list, and for when this fails to hold, functions `f` giving the head of
-- the list and `g` giving the seed from which to unfold the tail:
unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g b = if p b then Nil else Cons (f b) (unfoldL p f g (g b))
