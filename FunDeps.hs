{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}

module FunDeps where

data Z
data S n

class Plus m n r | m n -> r

instance Plus Z n n
instance Plus m n r => Plus (S m) n (S r)

two :: Plus (S Z) (S Z) r => r
two = undefined

three :: Plus (S Z) (S (S Z)) r => r
three = undefined
