> {-# LANGUAGE InstanceSigs #-}
> module Bits.Polarity where

> import Prelude hiding (Maybe(..), Ordering(..))

Types in a type signature can be in positive position or negative position

A type on its own is in positive postion, like

> i :: Int
> i = 42

> result :: Maybe String
> result = Just "yes"

> data Banana = Banana

> snacks :: [Banana]
> snacks = [Banana, Banana, Banana]

Function return types are in positive position, but parameters are in negative position

> listLength :: [a] -> Int
> listLength = length

[a] is in negative position, Int is positive position

> data Romulus = Romulus
> data Remus = Remus
> data Rome = Rome

> buildRome :: Romulus -> Remus -> Rome
> buildRome _ _ = Rome

Romulus and Resume are in negative position, Rome is in positive position.

For f to be an instance of Functor, every a in f a must be in positive position.

We say that "f is covariant in a"

> data Maybe a = Nothing | Just a

> instance Functor Maybe where
>   fmap :: (a -> b) -> Maybe a -> Maybe b
>   fmap _ Nothing  = Nothing
>   fmap f (Just x) = Just (f x)

> newtype Endo a = Endo (a -> a)

Endo is not a covariant functor, because a appears in both positive and negative position

We say that "Endo is invariant in a"

> newtype Predicate a = Predicate { runPredicate :: a -> Bool }

In Predicate, we only see a in negative position.

We say that "Predicate is contravariant in a".

> class Contravariant f where
>   contramap :: (b -> a) -> f a -> f b

We think of a covariant Functor as being full of a's.

A Contravariant functor can be though of as consuming a's.

> instance Contravariant Predicate where
>   contramap :: (b -> a) -> Predicate a -> Predicate b
>   contramap f (Predicate p) = Predicate (p . f)

> data Ordering = LT | EQ | GT

> newtype Comparison a = Comparison { runComparison :: a -> a -> Ordering }

> instance Contravariant Comparison where
>   contramap :: (b -> a) -> Comparison a -> Comparison b
>   contramap f (Comparison c) = Comparison (\a b -> c (f a) (f b))
