{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Singletons where

import           Data.Proxy
import           Prelude    hiding (head)

-- * Introduction

-- $
-- Some examples from Eisenberg and Weirich's
-- /Dependently Typed Programming with Singletons/.

data Nat :: * where
  Zero :: Nat
  Succ :: Nat -> Nat

deriving instance Show Nat

data Vec :: * -> Nat -> * where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

deriving instance Show a => Show (Vec a n)

type family   (m :: Nat) :< (n :: Nat) :: Bool
type instance m          :< 'Zero       = 'False
type instance 'Zero      :< ('Succ n)   = 'True
type instance ('Succ m)  :< ('Succ n)   = m :< n

-- $
-- To express the dependency between the value of one runtime argument and the
-- compile-time type of another requires the definition and use of singleton
-- types - types with only one non-bottom value.


-- |
-- The constructors of `SNat` mirror those of the kind `Nat`, and only one
-- non-bottom term exists in each fully-applied type in the `SNat` family.
--
-- Hence, types like `SNat` are called /singleton types/.
--
-- In such types, the type variable indexing the type and the one non-bottom
-- term of that type are always isomorphic.  Thus singleton types can be used to
-- force term-level computation and type-level computation to proceed in
-- lock-step.
--
data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- | This operation ensures that the index @m@ is less than the length of the
-- vector @n@ with the constraint @(m :< n) ~ True@.  Because this function
-- requires the index at runtime, this index cannot /only/ be a type.  We must
-- also include a runtime wtiness for this index, called a /singleton/, that can
-- be used for computation.  The type of singleton values for natural numbers is
-- `SNat`, a GADT indexed by a /type/ of /kind/ `Nat`.
--
-- >>> let exampleVec = VCons 1 (VCons 2 (VCons 3 VNil))
-- >>> nth (SSucc SZero) exampleVec
-- 2
--
-- This won't compile:
--
-- > exampleFail = nth (SSucc (SSucc (SSucc (SSucc SZero)))) exampleVec
--
nth :: (m :< n) ~ 'True => SNat m -> Vec a n -> a
nth SZero       (VCons a _)  = a
nth (SSucc sm') (VCons _ as) = nth sm' as


-- | A safe `head`
--
-- >>> let exampleVec = VCons 1 (VCons 2 (VCons 3 VNil))
-- >>> head exampleVec
-- 1
--
head :: Vec a ('Succ n) -> a
head (VCons h _) = h

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance 'Zero      :+ m           = m
type instance ('Succ n)  :+ m           = 'Succ (n :+ m)

-- |
-- >>> let x = VCons 1 (VCons 2 (VCons 3 VNil))
-- >>> let y = VCons 4 (VCons 5 (VCons 6 VNil))
-- >>> append x y
-- VCons 1 (VCons 2 (VCons 3 (VCons 4 (VCons 5 (VCons 6 VNil)))))
--
append :: Vec a n -> Vec a m -> Vec a (n :+ m)
append VNil        v2 = v2
append (VCons h t) v2 = VCons h (append t v2)

-- * Hasochism

-- $
-- Some more examples from Lindley and McBride's /Hasochism/.

-- |
-- >>> let x = VCons 1 (VCons 2 (VCons 3 (VCons 4 (VCons 5 (VCons 6 VNil)))))
-- >>> vchop (SSucc (SSucc (SSucc SZero))) x
-- (VCons 1 (VCons 2 (VCons 3 VNil)),VCons 4 (VCons 5 (VCons 6 VNil)))
--
vchop :: SNat m -> Vec x (m :+ n) -> (Vec x m, Vec x n)
vchop SZero xs               = (VNil,       xs)
vchop (SSucc m) (VCons x xs) = (VCons x ys, zs)
  where (ys, zs) = vchop m xs

-- |
-- >>> :set -XDataKinds
-- >>> let x = VCons 1 (VCons 2 (VCons 3 (VCons 4 (VCons 5 (VCons 6 VNil)))))
-- >>> let m = SSucc (SSucc (SSucc SZero))
-- >>> let n = Proxy :: Proxy ('Succ ('Succ ('Succ 'Zero)))
-- >>> vtake m n x
-- VCons 1 (VCons 2 (VCons 3 VNil))
--
vtake :: SNat m -> Proxy n -> Vec x (m :+ n) -> Vec x m
vtake SZero     _ _            = VNil
vtake (SSucc m) n (VCons x xs) = VCons x (vtake m n xs)

-- |
-- >>> :set -XDataKinds
-- >>> let x = VCons 1 (VCons 2 (VCons 3 (VCons 4 (VCons 5 (VCons 6 VNil)))))
-- >>> let m = SSucc (SSucc SZero)
-- >>> let n = SSucc (SSucc (SSucc (SSucc SZero)))
-- >>> let o = proxy n
-- >>> vtake m o x
-- VCons 1 (VCons 2 VNil)
--
proxy :: SNat i -> Proxy i
proxy _ = Proxy

-- | The Haskell analogue of an implicit Pi is constructed using singleton
-- classes
class SNAT (n :: Nat) where
  nat :: SNat n

instance SNAT 'Zero where
  nat = SZero

instance SNAT n => SNAT ('Succ n) where
  nat = SSucc nat

-- | A more implicit version of `vtake`.  The return type determines the
-- required length, so we can leave the business of singleton construction to
-- instance inference.
--
-- >>> :set -XDataKinds -XTypeFamilies
-- >>> vtrunc Proxy (VCons 1 (VCons 2 (VCons 3 (VCons 4 VNil)))) :: Vec Int (Succ (Succ Zero))
-- VCons 1 (VCons 2 VNil)
--
vtrunc :: SNAT m => Proxy n -> Vec x (m :+ n) -> Vec x m
vtrunc = vtake nat
