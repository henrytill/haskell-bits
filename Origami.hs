{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Origami
-- Description : Origami programming
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf>
--
module Origami where

import Data.Maybe (isNothing)

-- * Origami with lists: sorting

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- $setup
-- >>> let x = Cons 1 (Cons 2 (Cons 3 Nil))
-- >>> let y = Cons 4 (Cons 5 (Cons 6 Nil))

-- | A constructor for singleton `List`s
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

-- $
-- =Note:
-- Unfolds /generate/ data structures and folds /consume/ them.

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

-- $
-- A paramorphism captures a recursion pattern where the result depends not only
-- on a recursive call on a substructure, but also on the substructure itself.

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
-- =Note:
-- Sometimes it is convenient to provide the single argument of `unfoldL'` as
-- three components: a predicate indicating when that argument should return
-- `Nothing`, and two functions yielding the two components of the pair when it
-- does not.

unfoldL
  :: (b -> Bool) -- ^ Predicate `p` determines when the seed should unfold the empty `List`
  -> (b -> a)    -- ^ When @p == False@, `f` gives the head of the `List`
  -> (b -> b)    -- ^ When @p == False@, `g` gives the seed from which to unfold the tail
  -> b           -- ^ The thing to unfold
  -> List a      -- ^ The unfolded value
unfoldL p f g b = if p b
                    then Nil
                    else Cons (f b) (unfoldL p f g (g b))

-- *** Exercise 3.6
-- $ Express `unfoldL` in terms of `unfoldL'`, and vice versa

unfoldL1
  :: forall a b
   . (b -> Bool)
  -> (b -> a)
  -> (b -> b)
  -> b
  -> List a
unfoldL1 p f g = unfoldL' translator
  where
    translator :: b -> Maybe (a, b)
    translator x | p x       = Nothing
                 | otherwise = Just (f x, g x)

unfoldL2 :: forall a b. (b -> Maybe (a, b)) -> b -> List a
unfoldL2 h = unfoldL p f g
  where
    p :: b -> Bool
    p = isNothing . h

    f :: b -> a
    f x = case h x of
            Just (a, _) -> a
            Nothing     -> error "something is wrong"

    g :: b -> b
    g x = case h x of
            Just (_, b) -> b
            Nothing     -> error "something is wrong"

-- $
-- =Note:
-- Conversely, one could define a function `foldL'` taking a single argument of
-- type @Maybe (a, b) -> b@ in place of `foldL`'s two argument:

foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil         = f Nothing
foldL' f (Cons x xs) = f (Just (x, foldL' f xs))

-- *** Exercise 3.8
-- $ Define `foldL'` in terms of `foldL`, and vice versa.

-- foldL :: (a -> b -> b) -> b -> List a -> b

foldL1 :: forall a b. (Maybe (a, b) -> b) -> List a -> b
foldL1 f = foldL translator zero
  where
    translator :: a -> b -> b
    translator a b = f (Just (a, b))
    zero           = f Nothing

foldL2 :: forall a b. (a -> b -> b) -> b -> List a -> b
foldL2 f zero = foldL' translator
  where
    translator :: Maybe (a, b) -> b
    translator (Just (a, b)) = f a b
    translator Nothing       = zero

-- *** Exercise 3.9
-- $ The adaptation of the single-argument fold and unfold to the multi-argument
-- interface is simplified by functions of the following types:

foldLargs :: forall a b. (a -> b -> b) -> b -> (Maybe (a, b) -> b)
foldLargs f zero = translator
  where
    translator :: Maybe (a, b) -> b
    translator (Just (a, b)) = f a b
    translator Nothing       = zero

unfoldLargs
  :: forall a b
   . (b -> Bool)
  -> (b -> a)
  -> (b -> b)
  -> (b -> Maybe (a, b))
unfoldLargs p f g = translator
  where
    translator :: b -> Maybe (a, b)
    translator x | p x       = Just (f x, g x)
                 | otherwise = Nothing

-- $
-- One sorting algorithm expressible as a list unfold is /selection sort/, which
-- operates by at each step removing the minimum element of the list to be
-- sorted, but leaving the other elements in the same order.  We first define
-- the function /delmin/  to do this removal:

delmin :: Ord a => List a -> Maybe (a, List a)
delmin Nil = Nothing
delmin xs  = Just (y, deleteL y xs)
  where
    y = minimumL xs

-- $
-- `minimumL` and `deleteL` are `List` equivalents of the standard library
-- functions `minimum` and `delete`:

minimumL :: Ord a => List a -> a
minimumL Nil         = error "minimumL Nil"
minimumL (Cons x xs) = foldL min x xs

deleteL :: Eq a => a -> List a -> List a
deleteL _ Nil         = Nil
deleteL y (Cons x xs)
  | y == x            = xs
  | otherwise         = Cons x (deleteL y xs)

-- $
-- Then selection sort is straightforward to define:

-- |
-- >>> ssort (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
ssort :: Ord a => List a -> List a
ssort = unfoldL' delmin
