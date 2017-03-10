{-# LANGUAGE DeriveFunctor #-}

module BananasLensesEnvelopesBarbedWire where

import Prelude hiding (length, filter, zip, iterate, map)

data List a
  = Nil
  | Cons (a, (List a))
  deriving (Eq, Show, Functor)

xs, ys :: List Int
xs = Cons (1, Cons (2, Cons (3, Nil)))
ys = Cons (3, Cons (2, Cons (1, Nil)))

-- 1. Catamorphisms

type Cata a b = (b, a -> b -> b)

foldrList :: Cata a b -> List a -> b
foldrList   (nil, op) Nil            = nil
foldrList h@(nil, op) (Cons (a, as)) = a `op` (foldrList h as)

-- |
-- >>> length xs
-- 3
--
length = foldrList (0, op)
  where a `op` n = 1 + n

-- |
-- >>> filter odd xs
-- Cons (1,Cons (3,Nil))
--
filter p = foldrList (Nil, op)
  where a `op` as = if p a
                       then Cons (a, as)
                       else as


-- 2. Anamorphisms
--
-- Anamorphisms are not well-known in the functional programming folklore.  They
-- are called `unfold` by Bird & Wadler, who spend only a few words on them.

type Ana a b = (b -> (a, b), b -> Bool)

-- | Given a predicate `p` and a function `g`, a list-anamorphism is defined as:
unfold :: Ana a b -> b -> List a
unfold h@(g, p) b = if p b then Nil else Cons (a, unfold h b')
  where (a, b') = g b

-- | Zips a pair of `List`s into a `List` of pairs
--
-- >>> unfold zip (xs, ys)
-- Cons ((1,3),Cons ((2,2),Cons ((3,1),Nil)))
--
zip :: Ana (x, y) (List x, List y)
zip = (g, p)
  where
    p (Nil,  _) = True
    p (_ , Nil) = True
    p _         = False

    g (Cons (a, as), Cons (b, bs)) = ((a, b), (as, bs))

-- | Constructs the infinite list of iterated applications of `f` to `a`
iterate f = (g, (const False)) where g a = (a, f a)

-- | Applies `f` to every element in a given `List`.
--
-- >>> map (+1) xs
-- Cons (2,Cons (3,Cons (4,Nil)))
--
map f Nil            = Nil
map f (Cons (a, as)) = Cons (f a, map f as)

-- | Applies `f` to every element in a given `List` (Catamorphic version)
--
-- >>> cataMap (+1) xs
-- Cons (2,Cons (3,Cons (4,Nil)))
--
cataMap f = foldrList (Nil, op)
  where
    a `op` bs = Cons (f a, bs)

-- | Applies `f` to every element in a given `List` (Anamorphic version)
--
-- >>> anaMap (+1) xs
-- Cons (2,Cons (3,Cons (4,Nil)))
--
anaMap f = unfold (g, p)
  where
    p Nil = True
    p _   = False

    g (Cons (a, as)) = (f a, as)

-- 3. Hylomorphisms
--
-- A recursive function whose call-tree is isomorphic to a cons-list, i.e. a
-- linear recursive function, is called a /hylomorphism/.
--
-- A hylomorphism corresponds to the composition of an anamorphism that builds
-- the call-tree as an explicit data structure and a catamorphism that reduces
-- this data object to the required value.

type Hylo a b c = ((c, b -> c -> c), (a -> (b, a), a -> Bool))

foldAndUnfold :: Hylo a b c -> a -> c
foldAndUnfold h@((nil, op), (g, p)) a =
  if p a then nil else b `op` foldAndUnfold h a'
    where (b, a') = g a

-- | An archetypal hylomorphism is the `factorial` function:
--
-- >>> foldAndUnfold fac 5
-- 120
fac = ((1, (*)), (g, p))
  where
    p 0 = True
    p _ = False

    g n = (n, n - 1)

-- 4. Paramorphisms
--
-- ...cover (the) pattern of primitive recursion
--
-- Wikipedia:
--
-- A paramorphism is an extension of the concept of catamorphism first
-- introduced by Lambert Meertens to deal with a form which "eats its argument
-- and keeps it too", as exemplified by the factorial function.
--

type Para a b = (b, a -> b -> b)

numPara   (b, op) 0 = b
numPara h@(b, op) n = (n - 1) `op` (numPara h (n - 1))

-- |
-- >>> numPara fac' 5
-- 120
--
fac' = (1, op)
  where n `op` m = (1 + n) * m

listPara   (b, op) Nil            = b
listPara h@(b, op) (Cons (a, as)) = a `op` (as, listPara h as)

-- |
-- >>> listPara tails xs
-- Cons (Cons (1,Cons (2,Cons (3,Nil))),Cons (Cons (2,Cons (3,Nil)),Cons (Cons (3,Nil),Cons (Nil,Nil))))
--
tails = (Cons (Nil, Nil), op)
  where a `op` (as, tls) = Cons (Cons (a, as), tls)
