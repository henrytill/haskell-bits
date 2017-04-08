{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ApplicativeExamples where

import Prelude hiding (repeat, sequence)

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

-- * The `Applicative` class

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

type Matrix a = ZipList (ZipList a)

-- |
-- >>> transpose' (ZipList [ZipList [1,2,3], ZipList [4,5,6], ZipList [7,8,9]])
-- ZipList [ZipList [1,4,7],ZipList [2,5,8],ZipList [3,6,9]]
--
transpose' :: Matrix a -> Matrix a
transpose' (ZipList [])       = fmap ZipList (pure [])
transpose' (ZipList (xs:xss)) = fmap ZipList (pure (:) <*> xs <*> fmap unZipList (transpose' (ZipList xss)))
