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
import Prelude hiding (zip)

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
--
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

-- | The paramorphism operator for numbers
paraN :: (Eq a, Num a) => (a -> b -> b) -> b -> a -> b
paraN _  b 0 = b
paraN op b n = (n - 1) `op` (paraN op b (n - 1))

-- | Factorial defined using `paraN`
-- >>> factorial 5
-- 120
--
factorial :: (Num a, Eq a) => a -> a
factorial x = paraN op 1 x
  where
    n `op` m = (1 + n) * m

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
--
-- The Haskell standard library defines the follwoing function for generating
-- lists:
--
-- > unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
--

-- | An equivalent implementation of `unfoldr` for our `List` datatype
unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
                 Nothing     -> Nil
                 Just (x, v) -> Cons x (unfoldL' f v)

-- | `zip` in terms of `unfoldL'`
--
-- >>> zip (x, y)
-- Cons (1,4) (Cons (2,5) (Cons (3,6) Nil))
--
zip :: forall a b. (List a, List b) -> List (a, b)
zip = unfoldL' f
  where
    f :: (List a, List b) -> Maybe ((a, b), (List a, List b))
    f (Cons x xs, Cons y ys) = Just ((x, y), (xs, ys))
    f _                      = Nothing

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

-- $
-- Express `unfoldL` in terms of `unfoldL'`, and vice versa

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

-- $
-- These primed versions make the duality between the fold and the unfold very
-- clear, although they sometimes be less convenient for programming with.

-- *** Exercise 3.8

-- $
-- Define `foldL'` in terms of `foldL`, and vice versa.
--
-- > foldL :: (a -> b -> b) -> b -> List a -> b

foldL1 :: forall a b. (Maybe (a, b) -> b) -> List a -> b
foldL1 f = foldL translator zero
  where
    translator :: a -> b -> b
    translator a b = f (Just (a, b))
    zero           = f Nothing

foldL2 :: (a -> b -> b) -> b -> List a -> b
foldL2 f zero = foldL' (maybe zero (\ (a, b) -> f a b))

-- *** Exercise 3.9

-- $
-- The adaptation of the single-argument fold and unfold to the multi-argument
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
-- the function /delmin/ to do this removal:

-- |
-- >>> delmin (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Just (1,Cons 6 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil)))))
--
delmin :: Ord a => List a -> Maybe (a, List a)
delmin Nil = Nothing
delmin xs  = Just (y, deleteL y xs)
  where
    y = minimumL xs

-- | `minimumL` is the `List` equivalent of the standard library function
-- `minimum`
--
-- >>> minimumL (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- 1
--
minimumL :: Ord a => List a -> a
minimumL Nil         = error "minimumL Nil"
minimumL (Cons x xs) = foldL min x xs

-- | `deleteL` is the `List` equivalent of the standard library function
-- `delete`
--
-- >>> deleteL 6 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))
--
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

-- *** Exercise 3.10

-- | A redefinition of `deleteL` in terms of `paraL`
--
-- >>> deleteL' 6 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))
-- >>> deleteL' 5 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 6 (Cons 1 (Cons 2 (Cons 4 (Cons 3 Nil))))
--
deleteL' :: Eq a => a -> List a -> List a
deleteL' y = paraL f (wrap y)
  where
    f x (xs, acc)
      | y == x    = xs
      | otherwise = Cons x acc

-- *** Exercise 3.11

-- | A redefinition of `delmin` in terms of `paraL`
--
-- >>> delmin' (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Just (1,Cons 6 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil)))))
-- >>> delmin' (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 Nil))))))
-- Just (7,Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 Nil)))))
--
delmin' :: forall a. Ord a => List a -> Maybe (a, List a)
delmin' = paraL f Nothing
  where
    f :: a -> (List a, Maybe (a, List a)) -> Maybe (a, List a)
    f x (xs, Nothing)                   = Just (x, xs)
    f x (xs, Just (m, acc)) | x < m     = Just (x, xs)
                            | otherwise = Just (m, Cons x acc)

-- | `bubble` has the same type as `delmin`, but it does not preserve the
-- relative order of remaining list elements.  This means that it is possible to
-- define `bubble` as a fold.
--
bubble :: Ord a => List a -> Maybe (a, List a)
bubble = foldL step Nothing
  where
    step x Nothing        = Just (x, Nil)
    step x (Just (y, ys))
      | x < y             = Just (x, Cons y ys)
      | otherwise         = Just (y, Cons x ys)

-- |
-- >>> bsort (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
bsort :: Ord a => List a -> List a
bsort = unfoldL' bubble

-- *** Exercise 3.12

-- | An alternate version of `bubble` that returns a `List` with the minimum
-- element "bubbled" to the top
--
bubble' :: Ord a => List a -> List a
bubble' = foldL f Nil
  where
    f x Nil         = Cons x Nil
    f x (Cons m xs)
      | x < m       = Cons x (Cons m xs)
      | otherwise   = Cons m (Cons x xs)

-- |
-- >>> bsort' (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
--
bsort' :: Ord a => List a -> List a
bsort' = unfoldL' b
  where
    b xs = case bubble' xs of
             (Cons y ys) -> Just (y, ys)
             Nil         -> Nothing

-- *** Exercise 3.13

-- |
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insertWithUnfold 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
--
insertWithUnfold :: Ord a => a -> List a -> List a
insertWithUnfold x xs = unfoldL' inserter (Just x, xs)
  where
    inserter (Just i,  Cons y ys)
      | i < y                     = Just (i, (Nothing, Cons y ys))
      | otherwise                 = Just (y, (Just i,  ys))
    inserter (Nothing, Cons y ys) = Just (y, (Nothing, ys))
    inserter (Just i,  Nil)       = Just (i, (Nothing, Nil))
    inserter (Nothing, Nil)       = Nothing

-- *** Exercise 3.14

-- $
-- `insertWithUnfold` is a bit unsatisfactory, because once the correct position
-- is found at which to insert the element, the remainder of the list must still
-- be copied item by item.
--
-- The direct recursive definition did not have this problem: one branch shares
-- the remainder of the original list without making a recursive call.
--
-- This general pattern can be captured as another recursion operator, known as
-- an /apomorphism/.

-- | A function for apomorphisms.  For non-empty lists, the generation function
-- `f` yields `Either` a new seed, on which a recursive call is made, or a
-- complete list, which is used directly.
apoL' :: (b -> Maybe (a, Either b (List a))) -> b -> List a
apoL' f u = case f u of
              Nothing            -> Nil
              Just (x, Left v)   -> Cons x (apoL' f v)
              Just (x, Right xs) -> Cons x xs

-- | `insert` as an instance of `apoL'`.
--
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insertWithApo 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
--
insertWithApo :: forall a. Ord a => a -> List a -> List a
insertWithApo x xs = apoL' apper xs
  where
    apper :: (List a -> Maybe (a, Either (List a) (List a)))
    apper (Cons y ys)
      | y < x         = Just (y, Left ys)
      | otherwise     = Just (x, Right (Cons y ys))
    apper Nil         = Nothing

-- ** Hylomorphisms

-- $
-- Unfolds generate data structures, and folds consume them; it is natural to
-- compose these two operations.
--
-- The pattern of computation consisting of an unfold followed by a fold is a
-- fairly common one.  Such compositions are called /hylomorphisms/.
--
-- A simple example of a hylomorphism is given by the factorial function:

-- |
-- >>> fact 5
-- 120
--
fact :: Integer -> Integer
fact = foldL (*) 1 . unfoldL (== 0) id pred

-- $
-- More elaborate examples of hylomorphisms (on trees) are provided by
-- traditional compilers, which may be thought of as constructing an abstract
-- syntax tree (unfolding to the tree type) from which to generate code (folding
-- the abstract syntax tree).

hyloL
  :: (a -> c -> c)
  -> c
  -> (b -> Bool)
  -> (b -> a)
  -> (b -> b)
  -> b
  -> c
hyloL f e p g h = foldL f e . unfoldL p g h

-- |
-- >>> fact' 5
-- 120
--
fact' :: Integer -> Integer
fact' = hyloL (*) 1 (== 0) id pred

hyloLFused
  :: (a -> c -> c)
  -> c
  -> (b -> Bool)
  -> (b -> a)
  -> (b -> b)
  -> b
  -> c
hyloLFused f e p g h b =
  if p b
  then e
  else f (g b) (hyloLFused f e p g h (h b))

-- |
-- >>> factFused 5
-- 120
--
factFused :: Integer -> Integer
factFused = hyloL (*) 1 (== 0) id pred

-- *** Exercise 3.15

type Binary = List Bool

decimalStringToBinary :: String -> Binary
decimalStringToBinary = undefined

-- * Origami by numbers: loops

data Nat = Zero | Succ Nat deriving Show

-- ** Folds for naturals

foldN :: a -> (a -> a) -> Nat -> a
foldN z _ Zero     = z
foldN z s (Succ n) = s (foldN z s n)

-- $
-- If we reverse the order of the three arguments, we see that `foldN` is in
-- fact and old friend...

-- | A higher-order function that applies a given function of type @a -> a@ a
-- given number of times
--
iter :: Nat -> (a -> a) -> a -> a
iter n f x = foldN x f n

-- *** Exercise 3.16

-- | The single-argument version of foldN
foldN' :: (Maybe a -> a) -> Nat -> a
foldN' f Zero     = f Nothing
foldN' f (Succ n) = f (Just (foldN' f n))

-- | `foldN'` in terms of `foldN`
foldN1 :: (Maybe a -> a) -> Nat -> a
foldN1 f ns = foldN (f Nothing) (f . Just) ns

-- | `foldN` in terms of `foldN'`
foldN2 :: a -> (a -> a) -> Nat -> a
foldN2 z s = foldN' (maybe z s)

-- *** Exercise 3.18

-- |
-- >>> addN (Succ Zero) (Succ (Succ Zero))
-- Succ (Succ (Succ Zero))
--
addN :: Nat -> Nat -> Nat
addN m = foldN m Succ

-- |
-- >>> mulN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
--
mulN :: Nat -> Nat -> Nat
mulN m = foldN Zero (addN m)

-- |
-- >>> powN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
--
powN :: Nat -> Nat -> Nat
powN m = foldN (Succ Zero) (mulN m)

-- *** Exercise 3.19

predN :: Nat -> Maybe Nat
predN Zero     = Nothing
predN (Succ n) = Just n

-- | `predN` in terms of `foldN`
--
-- >>> predN' (Succ (Succ (Succ Zero)))
-- Just (Succ (Succ Zero))
-- >>> predN' Zero
-- Nothing
--
predN' :: Nat -> Maybe Nat
predN' = foldN Nothing f
  where
    f Nothing  = Just Zero
    f (Just n) = Just (Succ n)

-- *** Exercise 3.20

-- |
-- >>> subN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Just (Succ Zero)
--
subN :: Nat -> Nat -> Maybe Nat
subN m = foldN (Just m) ((=<<) predN)

-- |
-- >>> eqN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- False
-- >>> eqN (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))
-- True
--
eqN :: Nat -> Nat -> Bool
eqN (Succ m) (Succ n) = eqN m n
eqN Zero     (Succ _) = False
eqN (Succ _) Zero     = False
eqN Zero     Zero     = True

-- |
-- >>> lessN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- False
-- >>> lessN (Succ (Succ Zero)) (Succ (Succ Zero))
-- False
-- >>> lessN (Succ Zero) (Succ (Succ (Succ Zero)))
-- True
--
lessN :: Nat -> Nat -> Bool
lessN (Succ m) (Succ n) = lessN m n
lessN Zero     (Succ _) = True
lessN (Succ _) Zero     = False
lessN Zero     Zero     = False
