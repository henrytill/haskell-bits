{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}

module FunWithPhantomTypes where

-- * 1. Introducing "phantom types" (or GADTs)

-- | A simple expression language
data Term t where
  Zero   :: Term Int
  Succ   :: Term Int -> Term Int
  Pred   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

-- | An intepreter for the expression language
--
-- >>> let one = Succ Zero
-- >>> :type one
-- one :: Term Int
-- >>> eval one
-- 1
-- >>> eval (IsZero one)
-- False
-- >>> eval (If (IsZero one) Zero one)
-- 1
-- >>> let true = IsZero Zero
-- >>> let false = IsZero one
-- >>> eval (If true true false)
-- True
--
eval :: forall t. Term t -> t
eval (Zero)        = 0
eval (Succ e)      = eval e + 1
eval (Pred e)      = eval e - 1
eval (IsZero e)    = eval e == 0
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3

-- * 2. Generic functions

-- $generic_functions
-- Suppose you are developing an application where the need arises to compress
-- data to strings of bits.  As it happens, you have data of many different
-- types and you want to program a compression function that works for all of
-- these types.  This sounds like a typical case for Hakell's type classes.
-- Alas, I promised to do without type classes.  Fortunately, phantom types
-- offer an intriguing alternative.
--
-- The basic idea is to define a type whose elements represent types.  For
-- concreteness, assume that we need compression functions for types built from
-- 'Int' and 'Char' using the list and the pair type constructor.

data Type t where
  RInt  :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)

rString :: Type String
rString = RList RChar

data Bit = Ze | On

compress :: forall t. Type t -> t -> [Bit]
compress (RInt)        i      = compressInt i
compress (RChar)       c      = compressChar c
compress (RList _)     []     = Ze : []
compress (RList ra)    (a:as) = On : compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a, b) = compress ra a ++ compress rb b

compressInt :: Int -> [Bit]
compressInt = undefined

compressChar :: Char -> [Bit]
compressChar = undefined
