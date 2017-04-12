{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Fixed where

-- | the least fixpoint of functor f
newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance   Eq (f (Fix f)) =>   Eq (Fix f)
deriving instance  Ord (f (Fix f)) =>  Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

data ListF a r
  = N
  | C a r
  deriving (Eq, Show)

instance Functor (ListF a) where
  fmap f N        = N
  fmap f (C x xs) = C x (f xs)

data NatF r
  = Zero
  | Succ r
  deriving (Eq, Show, Functor)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

type List a = Fix (ListF a)

cons x xs = Fix (C x xs)
nil       = Fix N
