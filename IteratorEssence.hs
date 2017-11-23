{-# LANGUAGE ScopedTypeVariables #-}

module IteratorEssence where

import Prelude hiding (take, drop)

data Stream a = SCons a (Stream a)
  deriving (Eq, Show)

instance Functor Stream where
  fmap f (SCons x xs) = SCons (f x) (fmap f xs)

instance Applicative Stream where
  pure x                        = xs where xs = SCons x xs
  (SCons f fs) <*> (SCons x xs) = SCons (f x) (fs <*> xs)

drop :: (Eq a, Num a) => a -> Stream b -> Stream b
drop 0 (SCons _ xs) = xs
drop n (SCons _ xs) = drop (n - 1) xs

take :: (Eq a, Num a) => a -> Stream b -> [b]
take 0 _            = []
take n (SCons x xs) = x : take (n - 1) xs

lawIdentity :: Eq a => Stream a -> Bool
lawIdentity u
  = (pure id <*> u) == u

lawComposition
  :: (Eq a, Eq b, Eq c)
  => Stream (b -> c)
  -> Stream (a -> b)
  -> Stream a
  -> Bool
lawComposition u v w
  = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

lawHomomorphism :: Eq b => (a -> b) -> a -> Bool
lawHomomorphism f x
  = lhs f x == rhs f x
  where
    lhs :: (a -> b) -> a -> Stream b
    lhs f x = pure f <*> pure x

    rhs :: (a -> b) -> a -> Stream b
    rhs f x = pure (f x)

lawInterchange :: Eq b => Stream (a -> b) -> a -> Bool
lawInterchange u x
  = (u <*> pure x) == (pure (\ f -> f x) <*> u)
