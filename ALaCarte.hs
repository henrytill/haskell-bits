{-# OPTIONS_GHC -Wall              #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- |
-- Module      : ALaCarte
-- Description : Data types a la carte
-- Maintainer  : henrytill@gmail.com
-- Stability   : experimental
--
-- Code from Wouter Swiestra's
-- /Data types a la carte/.
--
module ALaCarte where

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators

-- * Fixing the expression problem

data Expr f = In (f (Expr f))

data Val e = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add


-- | The coproduct of two signatures.  Used for combining expressions.
infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

-- * Evaluation

instance Functor Val where
  fmap _ (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

-- |
-- >>> eval addExample
-- 1337
--
eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- * Automating injections

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-}
  (Functor f, Functor g) => f :<: (f :+: g) where
  inj         = Inl
  prj (Inl x) = Just x
  prj (Inr _) = Nothing

instance {-# OVERLAPPABLE #-}
  (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj         = Inr . inj
  prj (Inr x) = prj x
  prj (Inl _) = Nothing

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 `add`
add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add x y = inject (Add x y)

-- $
-- >>> let x :: Expr (Add :+: Val) = val 30000 `add` val 1330 `add` val 7
-- >>> eval x
-- 31337
--

-- * Examples

data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

infixl 7 `mul`
mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
mul x y = inject (Mul x y)

-- $
-- >>> let x :: Expr (Val :+: Add :+: Mul) = val 80 `mul` val 5 `add` val 4
-- >>> eval x
-- 404
-- >>> let y :: Expr (Val :+: Mul) = val 6 `mul` val 7
-- >>> eval y
-- 42

class Render f where
  render :: Render g  => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

-- $
-- >>> let x :: Expr (Val :+: Add :+: Mul) = val 80 `mul` val 5 `add` val 4
-- >>> pretty x
-- "((80 * 5) + 4)"

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

-- |
-- Applies the distributive law on the outermost constructors of an expression.
--
-- >>> let x :: Expr (Val :+: Add :+: Mul) = val 80 `mul` (val 5 `add` val 4)
-- >>> let Just y = distr x
-- >>> pretty y
-- "((80 * 5) + (80 * 4))"
distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do
  Mul a b <- match t
  Add c d <- match b
  return (a `mul` c `add` a `mul` d)

