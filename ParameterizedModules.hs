{-# LANGUAGE RankNTypes #-}

module ParameterizedModules where

data Mod = Mod
  { same :: forall a. Eq a => a -> a -> Bool
  , new  :: forall m a. (Monad m, Eq a) => a -> m a
  }

data Mod2 = Mod2
  { same2 :: forall a. Eq a => a -> a -> Bool
  , new2  :: forall a. Eq a => a -> a
  }

instance Show Mod where
  show _ = "<Mod>"


