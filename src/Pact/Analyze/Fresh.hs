{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-} -- for undetermined s in MonadState

module Pact.Analyze.Fresh
  ( MonadFresh (gen)
  , FreshT
  , runFreshT
  , evalFreshT
  , execFreshT
  , ConstFreshT
  , runConstFreshT
  ) where

import           Control.Monad.Except        (ExceptT, MonadError)
import           Control.Monad.Reader        (MonadReader (ask, reader, local),
                                              ReaderT, mapReaderT, runReaderT)
import qualified Control.Monad.State.Lazy    as LS
import           Control.Monad.State.Strict  (MonadState (get, put, state),
                                              StateT, modify, evalStateT,
                                              execStateT, runStateT)
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Maybe   (MaybeT)
import           Control.Monad.Writer.Strict (WriterT, MonadWriter)
import qualified Control.Monad.Writer.Lazy   as LW

-- NOTE: Indexed by the ID type so users can generate more than one type of ID
--       in a program.
class Monad m => MonadFresh i m where
  gen :: m i

  default gen :: (MonadTrans t, MonadFresh i m', m ~ t m') => m i
  gen = lift gen

instance MonadFresh i m             => MonadFresh i (ExceptT e m)
instance MonadFresh i m             => MonadFresh i (MaybeT m)
instance MonadFresh i m             => MonadFresh i (ReaderT r m)
instance MonadFresh i m             => MonadFresh i (StateT s m)
instance MonadFresh i m             => MonadFresh i (LS.StateT s m)
instance (MonadFresh i m, Monoid w) => MonadFresh i (WriterT w m)
instance (MonadFresh i m, Monoid w) => MonadFresh i (LW.WriterT w m)

-- * Standard/auto-incrementing fresh ID generation

newtype FreshT i m a = FreshT { unFreshT :: StateT i m a }
  deriving (Functor, Applicative, Monad, MonadTrans,
            MonadError e, MonadReader r, MonadWriter w)

-- Allow another MonadFresh in a stack if the ID types don't conflict
instance {-# OVERLAPPABLE #-} MonadFresh i m => MonadFresh i (FreshT i' m)

-- Have to define this one by-hand, because we use StateT in the implementation
instance MonadState s m => MonadState s (FreshT i m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monad m, Enum id) => MonadFresh id (FreshT id m) where
  gen = FreshT $ do
    next <- get
    modify succ
    pure next

runFreshT :: FreshT i m a -> i -> m (a, i)
runFreshT = runStateT . unFreshT

evalFreshT :: Monad m => FreshT i m a -> i -> m a
evalFreshT = evalStateT . unFreshT

execFreshT :: Monad m => FreshT i m a -> i -> m i
execFreshT = execStateT . unFreshT

-- * Degenerate/constant "fresh" ID generation

newtype ConstFreshT i m a = ConstFreshT { unConstFreshT :: ReaderT i m a }
  deriving (Functor, Applicative, Monad, MonadTrans,
            MonadError e, MonadWriter w, MonadState s)

-- Allow another MonadFresh in a stack if the ID types don't conflict
instance {-# OVERLAPPABLE #-} MonadFresh i m => MonadFresh i (ConstFreshT i' m)

mapConstFreshT :: (m a -> n b) -> ConstFreshT i m a -> ConstFreshT i n b
mapConstFreshT f (ConstFreshT act) = ConstFreshT $ mapReaderT f act

-- Have to define this one by-hand, because we use ReaderT in the implementation
instance MonadReader r m => MonadReader r (ConstFreshT i m) where
  ask = lift ask
  reader = lift . reader
  local f = mapConstFreshT (local f)

instance Monad m => MonadFresh id (ConstFreshT id m) where
  gen = ConstFreshT ask

runConstFreshT :: ConstFreshT i m a -> i -> m a
runConstFreshT = runReaderT . unConstFreshT




-- ---- * DEMO * -------
--
-- newtype VarId = VarId Int deriving Enum
-- newtype TagId = TagId Int deriving Enum
--
-- -- Fresh name generation for var IDs and tag IDs
-- newtype App1 a = App1 (FreshT VarId (FreshT TagId (StateT Int IO)) a)
--   deriving (Functor, Applicative, Monad,
--             MonadState Int, MonadFresh VarId, MonadFresh TagId)
--
-- -- Fresh name generation for var IDs; constant/stubbed "generation" for tag IDs
-- newtype App2 a = App2 (FreshT VarId (ConstFreshT TagId (StateT Int IO)) a)
--   deriving (Functor, Applicative, Monad,
--             MonadState Int, MonadFresh VarId, MonadFresh TagId)

