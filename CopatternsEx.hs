{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module CopatternsEx where

import Prelude hiding (pred)

data Enumeration a = Enumeration
  { start    :: a
  , forward  :: a -> a
  , backward :: a -> a
  }

third :: Enumeration a -> a
third e = forward e (forward e (forward e (start e)))

backwardTwo :: Enumeration a -> a -> a
backwardTwo e a = backward e (backward e a)

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Show, Eq)

enumNat :: Enumeration Nat
enumNat = Enumeration
  { start    = Z
  , forward  = S
  , backward = pred
  }
  where
    pred :: Nat -> Nat
    pred Z     = Z
    pred (S n) = n

test1 :: Bool
test1 = third enumNat == S (S (S Z))

test2 :: Bool
test2 = backwardTwo enumNat (S (S (S (S (S Z))))) == S (S (S Z))
