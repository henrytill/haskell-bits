{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : AgdaLens
-- Description : A cut-down implementation of lenses
-- Stability   : experimental
--
-- A cut-down implementation of lenses, with names taken from Edward Kmett's lens package.
--
-- <https://github.com/agda/agda/blob/master/src/full/Agda/Utils/Lens.hs>
--
module AgdaLens where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map

-- | The true pure @for@ loop.
--
-- 'Data.Traversable.for' is a misnomer, it should be @forA@.
--
for :: Functor m => m a -> (a -> b) -> m b
for = flip fmap

infix 4 <&>

-- | Infix version of 'for'.
(<&>) :: Functor m => m a -> (a -> b) -> m b
(<&>) = for

-- * Type-preserving lenses.

-- | Van Laarhoven style homogeneous lenses.
-- Mnemoic: "Lens inner outer".
type Lens' i o = forall f. Functor f => (i -> f i) -> o -> f o

-- * Some simple lenses.

lFst :: Lens' a (a, b)
lFst f (x, y) = (, y) <$> f x

lSnd :: Lens' b (a, b)
lSnd f (x, y) = (x,) <$> f y

-- * Elementary lens operations.

infixl 8 ^.

-- | Get inner part @i@ of structure @o@ as designated by @Lens' i o@.
(^.) :: o -> Lens' i o -> i
o ^. l = getConst $ l Const o

-- | Set inner part @i@ of structure @o@ as designated by @Lens' i o@.
set :: Lens' i o -> i -> o -> o
set l = over l . const

-- | Modify inner part @i@ of structure @o@ using a function @i -> i@.
over :: Lens' i o -> (i -> i) -> o -> o
over l f o = runIdentity $ l (Identity . f) o

-- * State accessors and modifiers.

-- | Read a part of the state.
use :: MonadState o m => Lens' i o -> m i
use l = do !x <- gets (^. l)
           return x

infix 4 .=

-- | Write a part of the state.
(.=) :: MonadState o m => Lens' i o -> i -> m ()
l .= i = modify $ set l i

infix 4 %=

-- | Modify a part of the state.
(%=) :: MonadState o m => Lens' i o -> (i -> i) -> m ()
l %= f = modify $ over l f

infix 4 %==

-- | Modify a part of the state monadically.
(%==) :: MonadState o m => Lens' i o -> (i -> m i) -> m ()
l %== f = put =<< l f =<< get

infix 4 %%=

-- | Modify a part of the state monadically, and return some result.
(%%=) :: MonadState o m => Lens' i o -> (i -> m (i, r)) -> m r
l %%= f = do
  o <- get
  (o', r) <- runWriterT $ l (WriterT . f) o
  put o'
  return r

-- * Read-only state accessors and modifiers.

-- | Ask for part of read-only state.
view :: MonadReader o m => Lens' i o -> m i
view l = asks (^. l)

-- | Modify a part of the state in a subcomputation.
locally :: MonadReader o m => Lens' i o -> (i -> i) -> m a -> m a
locally l = local . over l

key :: Ord k => k -> Lens' (Maybe v) (Map k v)
key k f m = f (Map.lookup k m) <&> \ v -> Map.alter (const v) k m
