{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Despiser where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import qualified Database.SQLite.Simple as SQLite

type Name = T.Text

type Value = [T.Text]

type EvalScope = HML.HashMap Name Value

data EvalState = EvalState
  { _global :: EvalScope
  , _stack  :: [EvalScope]
  }

data EvalCtx = EvalCtx
  { _interactive :: Bool
  , _conn        :: SQLite.Connection
  }

newtype Eval a = Eval { unEval :: ReaderT EvalCtx (StateT EvalState (ResourceT IO)) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadReader EvalCtx,
            MonadState EvalState)

runEval :: EvalCtx -> EvalState -> Eval a -> IO (a, EvalState)
runEval ro rw m = runResourceT (runStateT (runReaderT (unEval m) ro) rw)
