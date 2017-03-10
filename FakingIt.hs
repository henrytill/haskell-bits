{-# LANGUAGE FunctionalDependencies #-}

module FakingIt where

data Zero = Zero   deriving Show
data Suc n = Suc n deriving Show

class Nat n
instance Nat Zero
instance Nat n => Nat (Suc n)

one = Suc Zero
two = Suc (Suc Zero)
