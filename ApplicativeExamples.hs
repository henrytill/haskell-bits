{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ApplicativeExamples where

import Prelude hiding (any, concat, elem, repeat, sequence, traverse, Traversable)

-- * 1. Introduction

-- ** Sequencing commands

-- $
-- One often wants to execute a sequence of commands and collect the sequence
-- of their responses.
--
-- Indeed, there is such a function in the Haskell Prelude (here specialised to `IO`):
--
-- > sequence :: [IO a] -> IO [a]
-- > sequence []     = return []
-- > sequence (c:cs) = do
-- >   x <- c
-- >   xs <- sequence cs
-- >   return (x:xs)
--
-- In the @ (c : cs) @ case, we collect values of some effectful computations,
-- which we then use as the arguments to a pure function @ (:) @.
--
-- We could avoid the need for names to wire these values through to their point
-- of usage if we had a kind of "effectful application".

-- | "Effectful application"
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return (f x)

-- | Implementation of `sequence` using `return` and `ap`
--
-- `return` lifts pure values to the effecful world, whilst `ap` provides "application" within it
--
sequence :: [IO a] -> IO [a]
sequence []     = return []
sequence (c:cs) = return (:) `ap` c `ap` sequence cs

-- ** Transposing "matrices"

-- $
-- Suppose we represent matrices by lists of lists.  A common operation on
-- matrices is transposition.
--
-- > transpose :: [[a]] -> [[a]]
-- > transpose []       = repeat []
-- > transpose (xs:xss) = zipWith (:) xs (transpose xss)
--
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

-- $
-- The binary `zipWith` is one of a family of operations that "vectorise" pure
-- functions.
--
-- As Daniel Fridlender and Mia Indrika point out, the entire family can be
-- generated from `repeat` and `zapp`.

-- | Generates an infinite stream from its argument
repeat :: a -> [a]
repeat x = x : repeat x

-- | Zippy application
zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _      _      = []

-- | An implementation of `transpose` using `repeat` and `zapp`.
transpose :: [[a]] -> [[a]]
transpose []       = repeat []
transpose (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss

-- ** Evaluating expressions

data Exp v
  = Var v
  | Val Int
  | Add (Exp v) (Exp v)

type Env v = [(v, Int)]

fetch :: Eq v => v -> Env v -> Int
fetch v env = case lookup v env of
                Just x  -> x
                Nothing -> error "whoops"

-- $
-- When implementing an evaluator for a language of expressions, it is customary
-- to pass around an environment, giving values to the free variables.
--
-- > eval :: Eq v => Exp v -> Env v -> Int
-- > eval (Var x)   env = fetch x env
-- > eval (Val i)   env = i
-- > eval (Add p q) env = eval p env + eval q env
--
-- We can eliminate the clutter of the explicitly threaded environment with a
-- little help from some very old friends, designed for this purpose:

k :: a -> env -> a
k x env = x

s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es env = (ef env) (es env)

eval :: Eq v => Exp v -> Env v -> Int
eval (Var x)   = fetch x
eval (Val i)   = k i
eval (Add p q) = k (+) `s` eval p `s` eval q

-- $
-- This definition of `eval` is in a fairly standard applicative style, even
-- though we are abstracting an environment.

-- * 2. The `Applicative` class

-- $
-- > infixl 4 (<*>)
-- >
-- > class Applicative f where
-- >   pure  :: a -> fa
-- >   (<*>) :: f (a -> b) -> f a -> f b
--
-- This class generalizes `s` and `k` from threading an environment to
-- threading an effect in general.
--
-- We shall require the following laws for applicative functors:
--
-- __identity__
--
-- > pure id <*> u = u
--
-- __composition__
--
-- > pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- __homomorphism__
--
-- > pure f <*> pure x = pure (f x)
--
-- __interchange__
--
-- > u <*> pure x = pure (\ f -> f x) <*> u
--
-- The idea is that `pure` embeds pure computations into the pure fragment of an
-- effectful world - the result computations may thus be shunted around freely,
-- as long as the order of the genuinely effectful computations is preserved.
--
-- You can easily check that applicative functors are indeed functors, with the
-- following actions on function:
--
-- > (<$>) :: Applicative f => (a -> b) -> f a -> f b
-- > f <$> u = pure f <*> u
--
-- Morever any expression built from the `Applicative` combinators can be
-- transformed to a canonical form in which a single pure function is "applied"
-- to the effectful parts in depth-first-order:
--
-- > pure f <*> a_1 <*> ... <*> a_n
--
-- This canonical form captures the essence of `Applicative` programming:
-- computations have a fixed structure, given by the pure function, and a
-- sequence of sub-computations, given by the effectful arguments.
--
-- Any `Monad` can be made `Applicative` by taking
--
-- > pure  = return
-- > (<*>) = ap
--
-- Sometimes we can implment the `Applicative` interface a little more directly:
--
-- > instance Applicative ((->) env) where
-- >   pure x    = \env -> x
-- >   ef <*> ex = \env -> (ef env) (ex env)

sequence' :: [IO a] -> IO [a]
sequence' []     = pure []
sequence' (c:cs) = pure (:) <*> c <*> sequence' cs

eval' :: Eq v => Exp v -> Env v -> Int
eval' (Var x)   = fetch x
eval' (Val i)   = pure i
eval' (Add p q) = pure (+) <*> eval' p <*> eval' q

newtype ZipList a = ZipList { unZipList :: [a] }
  deriving Functor

instance Show a => Show (ZipList a) where
  show (ZipList a) = "ZipList " ++ show a

instance Applicative ZipList where
  pure    = ZipList . repeat
  a <*> b = ZipList (zapp (unZipList a) (unZipList b))

-- |
-- >>> transpose' [[1,2,3], [4,5,6], [7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
--
transpose' :: [[a]] -> [[a]]
transpose' = unZipList . appDist . fmap ZipList

-- * 3. Traversing data structures

-- | The /applicative distributor/, of which `sequence` and `transpose` are both
-- instances
appDist :: Applicative f => [f a] -> f [a]
appDist []       = pure []
appDist (xs:xss) = pure (:) <*> xs <*> appDist xss

-- $
-- Distribution is often used together with "map".
--
-- For example, given the monadic "failure-propagation" applicative functor for
-- `Maybe`, we can map some failure-prone operation (a function @ a -> Maybe b
-- @) across a list of inputs in such a way that any individual failure causes
-- failure overall.
--
-- > flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
-- > flakyMap f ss = dist (fmap f ss)
--
-- As you can see `flakyMap` traverses /ss/ twice - once to apply /f/, and again
-- to collect the results.  More generally it is preferable to define this
-- applicative mapping operation directly, with a single traversal:

rawTraverse :: Applicative f => (a -> f b) -> [a] -> f [b]
rawTraverse f []     = pure []
rawTraverse f (x:xs) = pure (:) <*> f x <*> rawTraverse f xs

-- $
-- This is just the way that you would implement ordinary `fmap` for lists, but
-- the right-hand sides are shifted into the idiom.

class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a     -> f (t b)
  dist     :: Applicative f =>               t (f a) -> f (t a)
  dist      = traverse id

instance Traversable [] where
  traverse = rawTraverse

-- $
-- Of course we can recover an oridinary "map" operator by taking /f/ to be the
-- identity - the simple applicative functor in which all computations are pure:

newtype Id a = An { an :: a }
  deriving Show

instance Functor Id where
  fmap f (An x) = An (f x)

instance Applicative Id where
  pure          = An
  An f <*> An x = An (f x)

-- $
-- So with the __newtype__ signalling which `Applicative` functor to thread, we have:

-- |
-- >>> effmap (\x -> (x + 1)) [1,2,3]
-- [2,3,4]
effmap :: Traversable t => (a -> b) -> t a -> t b
effmap f = an . traverse (An . f)

-- $
-- The rule of thumb for `traverse` is "like `fmap` but with (idioms) on the
-- right"

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

instance Traversable Tree where
  traverse f Leaf         = pure Leaf
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r

-- $
-- >>> let t = Node Leaf 43 (Node (Node Leaf 45 Leaf) 47 (Node (Node Leaf 49 Leaf) 51 Leaf))
-- >>> traverse (\ x -> An (x * 3)) t
-- An {an = Node Leaf 129 (Node (Node Leaf 135 Leaf) 141 (Node (Node Leaf 147 Leaf) 153 Leaf))}

-- * 4. `Monoid`s are phantom `Applicative` functors

newtype Accy o a = Acc { acc :: o }
  deriving Show

instance Functor (Accy o) where
  fmap _ (Acc acc) = Acc acc

-- $
-- @ Accy o a @ is a /phantom type/ - its values have nothing to do with /a/, but
-- it does yield the applicative functor of accumulating computations:

instance Monoid o => Applicative (Accy o) where
  pure _          = Acc mempty
  Acc a <*> Acc b = Acc (a `mappend` b)

-- $
-- Now reduction or "crushing" is just a special kind of traversal, in the same
-- way as with any other applicative functor.

accumulate :: (Traversable t, Monoid o) => (a -> o) -> t a -> o
accumulate f = acc . traverse (Acc . f)

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = accumulate id

-- $
-- Operations like tree flattening and concatenation become straightforward:

-- |
-- >>> let t = Node Leaf 43 (Node (Node Leaf 45 Leaf) 47 (Node (Node Leaf 49 Leaf) 51 Leaf))
-- >>> flatten t
-- [43,45,47,49,51]
--
flatten :: Tree a -> [a]
flatten = accumulate (: [])

-- |
-- >>> concat [[1,2],[3,4],[5,6]]
-- [1,2,3,4,5,6]
--
concat :: [[a]] -> [a]
concat = reduce

-- $
-- We can extract even more work from instance inference if we use the type
-- system to distinguish different monoids available for a given datatype.  Here
-- we use the disjunctive structure of `Bool` to test for the presence of an
-- element satisfying a given predicate:

newtype Mighty = Might { might :: Bool }

instance Monoid Mighty where
  mempty                    = Might False
  Might x `mappend` Might y = Might (x || y)

-- |
-- >>> any (== 1) [1,2,3,4]
-- True
-- >>> any (== 5) [1,2,3,4]
-- False
--
any :: Traversable t => (a -> Bool) -> t a -> Bool
any p = might . accumulate (Might . p)

-- | @ any . (==) @ behaves just as the the `elem` function for lists
--
-- >>> elem 1 [1,2,3,4]
-- True
-- >>> elem 5 [1,2,3,4]
-- False
--
elem :: (Eq a, Traversable t) => a -> t a -> Bool
elem = any . (==)

-- $
-- `any` can also tell whether a variable from /v/ occurs free in an `Exp v`.
-- Of course, `Bool` also has a conjunctive `Must`y structure, which is just as
-- easy to exploit.

-- * 5. `Applicative` versus `Monad`?

-- ** Composing applicative functors

-- $
-- The `Applicative` class is /closed under composition/.

newtype Comp f g a = Comp { comp :: f (g a) }

instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap f (Comp x) = Comp (fmap (fmap f) x)

-- $
-- ...just by lifting the inner `Applicative` operations to the outer layer:

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
  pure x              = Comp (pure (pure x))
  Comp fs <*> Comp xs = Comp ((pure (<*>)) <*> fs <*> xs)

-- ** Accumulating exceptions

data Except err a = OK a | Failed err
  deriving Show

instance Functor (Except err) where
  fmap _ (Failed err) = Failed err
  fmap f (OK a)       = OK (f a)

-- $
-- A `Monad` instance for this type must abort the computation on the first
-- error, as there is then no value to pass to the second argument of `>>=`.
-- However with the `Applicative` interface we can continue in the face or
-- errors:

instance Monoid err => Applicative (Except err) where
  pure                        = OK
  OK f        <*> OK x        = OK (f x)
  OK f        <*> Failed err  = Failed err
  Failed err  <*> OK x        = Failed err
  Failed err1 <*> Failed err2 = Failed (err1 `mappend` err2)

-- $
-- >>> pure (\x y z -> x + y + z) <*> OK 2 <*> Failed ["quux"] <*> Failed ["baz"]
-- Failed ["quux","baz"]
