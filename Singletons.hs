{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}

module Singletons where

import Prelude hiding (head)

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
-- To express the dependency between the vale of one runtime argument and the
-- compile-time type of another requires the definition and use of /singleton
-- types/ - types with only one non-bottom value.

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

-- $
-- Because the constructors of `SNat` mirror those of the kind `Nat`, only one
-- non-bottom term exists in each fully-applied type in the `SNat` family.
-- Hence, these types are called singleton types.  In such types, the type
-- variable indexing the type and th one non-bottom term of that type are always
-- isomorphic.  Thus singleton types can be used to force term-level computation
-- and type-level computation to proceed in lock-step.

-- | A safe `head`
--
-- >>> let exampleVec = VCons 1 (VCons 2 (VCons 3 VNil))
-- >>> head exampleVec
-- 1
--
head :: Vec a ('Succ n) -> a
head (VCons h _) = h

type family   Plus (n :: Nat) (m :: Nat) :: Nat
type instance Plus 'Zero      m           = m
type instance Plus ('Succ n)  m           = 'Succ (Plus n m)

-- |
-- >>> let x = VCons 1 (VCons 2 (VCons 3 VNil))
-- >>> let y = VCons 4 (VCons 5 (VCons 6 VNil))
-- >>> append x y
-- VCons 1 (VCons 2 (VCons 3 (VCons 4 (VCons 5 (VCons 6 VNil)))))
--
append :: Vec a n -> Vec a m -> Vec a (Plus n m)
append VNil        v2 = v2
append (VCons h t) v2 = VCons h (append t v2)
