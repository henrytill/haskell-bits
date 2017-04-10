{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FirstLenses where

-- * Motivation

-- $setup
-- >>> let addr = A { _road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "02134"}
-- >>> let fred = P { _name = "Fred", _addr = addr, _salary = 100 }

data Person
  = P { _name   :: String
      , _addr   :: Address
      , _salary :: Int
      } deriving Show

data Address
  = A { _road     :: String
      , _city     :: String
      , _postcode :: String
      } deriving Show

-- $Why
-- This sort of code gets __tiresome__ really fast:
--
-- > setName :: String -> Person -> Person
-- > setName n p = p { name = n }
--
-- > setPostcode :: String -> Person -> Person
-- > setPostcode pc p = p { addr = (addr p) { postcode = pc } }

-- * Edward's Insight

-- $
-- One function to rule them all!

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- $Want
--
-- A lens for each field:
--
-- > lname   :: Lens' Person String
-- > laddr   :: Lens' Person Address
-- > lsalary :: Lens' Person Int
--
-- A way to use the lens to get and update:
--
-- > view :: Lens' s a -> s -> a
-- > set  :: Lens' s a -> a -> s -> s
--
-- A way to compose lenses:
--
-- > composeL :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a
--
-- If we had that...
--
-- > setPostcode :: String -> Person -> Person
-- > setPostcode pc p = set (laddr `composeL` lpostcode) pc p
--

-- | A type which is isomorphic to `Lens'`
--
-- > lensToLensR :: Lens' s a -> LensR s a
-- > lensRToLens :: LensR s a -> Lens' s a
data LensR s a
  = L { viewR :: s -> a
      , setR  :: a -> s -> s
      }

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- |
-- Point-free translation:
--
-- > set :: Lens' s a -> (a -> s -> s)
-- > set ln x = runIdentity . ln (Identity . const x)
--
set :: Lens' s a -> (a -> s -> s)
set ln x s = runIdentity (ln set_fld s)
  where
    set_fld _ = Identity x

over :: Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

-- | A functor that ignores its argument
newtype Const v a = Const { getConst :: v }

instance Functor (Const v) where
  fmap _ (Const x) = Const x

-- |
-- Point-free translation:
--
-- > view :: Lens' s a -> (s -> a)
-- > view ln = getConst . ln Const
--
view :: Lens' s a -> s -> a
view ln s = getConst (ln Const s)

lensToLensR :: Lens' s a -> LensR s a
lensToLensR ln = L { viewR = view ln, setR = set ln }

lensRToLens :: LensR s a -> Lens' s a
lensRToLens L{..} f s = (\a -> setR a s) <$> (f (viewR s))

-- * Making lenses

-- |
-- >>> view name fred
-- "Fred"
-- >>> set name "Bill" fred
-- P {_name = "Bill", _addr = A {_road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "02134"}, _salary = 100}
--
name :: Lens' Person String
name elt_fn (P n a s) = (\n' -> P n' a s) <$> (elt_fn n)

-- * How on earth does this work?

-- $
--
-- > view name (P {_name = "Fred", _addr = ..., _salary = 100})
-- > getConst (name Const (P {_name = "Fred", _addr = ..., _salary = 100})) -- inline view
-- > getConst (fmap (\n' -> P n' A{...} 100) (Const "Fred"))                -- inline name
-- > getConst (Const "Fred")
-- > "Fred"
--
-- The __newtype__ has no runtime cost!
--
-- It just tells the @Functor f =>@ which functor dictionary to pass to `ln`.

-- * Composing and using lenses

address :: Lens' Person Address
address elt_fn (P n a s) = (\a' -> P n a' s) <$> (elt_fn a)

-- |
-- >>> view (address . postcode) fred
-- "02134"
--
postcode :: Lens' Address String
postcode elt_fn (A r c p) = (\p' -> A r c p') <$> (elt_fn p)

-- |
-- >>> setPostcode "55555" fred
-- P {_name = "Fred", _addr = A {_road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "55555"}, _salary = 100}
--
setPostcode :: String -> Person -> Person
setPostcode = set (address . postcode)

-- * Edward's Second Insight

-- $
-- Change `Functor` to `Applicative` and get a multi-focus lens

type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

roadAndCity :: Traversal' Address String
roadAndCity elt_fn (A r c p) = (\r' c' -> A r' c' p) <$> elt_fn r <*> elt_fn c
