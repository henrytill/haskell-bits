{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NatFun where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import Prelude hiding (Double)

type family Halve (n :: Nat) :: Nat where
  Halve 0 = 0
  Halve 1 = 0
  Halve n = 1 + Halve (n - 2)

type family Double (n :: Nat) :: Nat where
  Double 0 = 0
  Double n = 2 + Double (n - 1)

proofHalve2 :: Halve 2 :~: 1
proofHalve2 = Refl

proofHalve4 :: Halve 4 :~: 2
proofHalve4 = Refl

proofDouble2 :: Double 2 :~: 4
proofDouble2 = Refl

proofHalve6Step :: Halve 6 :~: 1 + Halve 4
proofHalve6Step = Refl

proofHalve6Plus :: Halve 6 :~: 1 + 2
proofHalve6Plus = Refl

proof1Plus2 :: 1 + 2 :~: 3
proof1Plus2 = Refl

proofHalve6 :: Halve 6 :~: 3
proofHalve6 = trans proofHalve6Step (trans proofHalve6Plus proof1Plus2)

proofSymHalve2 :: 1 :~: Halve 2
proofSymHalve2 = sym proofHalve2

proofSymHalve4 :: 2 :~: Halve 4
proofSymHalve4 = sym proofHalve4

proofSymHalve6 :: 3 :~: Halve 6
proofSymHalve6 = sym proofHalve6

proofComplexSym :: 3 :~: 1 + Halve 4
proofComplexSym = trans (sym proofHalve6) proofHalve6Step

halve :: forall n. (KnownNat n, KnownNat (Halve n)) => Proxy n -> Integer
halve _ = natVal (Proxy @(Halve n))

main :: IO ()
main = do
  putStrLn $ "Halve 4 = " ++ show (halve (Proxy @4))
  putStrLn $ "Halve 5 = " ++ show (halve (Proxy @5))
  putStrLn $ "Halve 6 = " ++ show (halve (Proxy @6))
