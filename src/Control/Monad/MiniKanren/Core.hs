
{-#Language GeneralizedNewtypeDeriving #-}

module Control.Monad.MiniKanren.Core 
  ( LVar, lvarKey, lvarValue
  , MiniKanren, MiniKanrenT, Unifiable(..)
  , run, runAll, runT, runAllT
  , MonadKanren(..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Logic
import Control.Monad.LogicVarT

import Data.Data
import Data.Functor.Identity

class (Data a) => Unifiable a where
  unifyValue :: (Monad m) => a -> a -> MiniKanrenT m ()

newtype MiniKanrenT m a = MiniKanrenT (LogicVarT (LogicT m) a)
  deriving (Monad, Applicative,
            Alternative, Functor, MonadPlus, MonadIO, MonadLogic)

instance MonadTrans MiniKanrenT where
  lift m = MiniKanrenT $ lift $ lift m


newtype MiniKanren a = MiniKanren (MiniKanrenT Identity a)
  deriving (Monad, Applicative,
            Alternative, Functor, MonadPlus, MonadLogic, MonadKanren)


runAll :: (Data a) => MiniKanren a -> [a]
runAll (MiniKanren m) = runIdentity $ runAllT m

run :: (Data a) => Int -> MiniKanren a -> [a]
run n (MiniKanren m) = runIdentity $ runT n m

runT :: (Monad m, Data a) => Int -> MiniKanrenT m a -> m [a]
runT n (MiniKanrenT m) = observeManyT n (runLogicVarT (m >>= unrollLVars))

runAllT :: (Monad m, Data a) => MiniKanrenT m a -> m [a]
runAllT (MiniKanrenT m) = observeAllT (runLogicVarT (m >>= unrollLVars))


class (MonadLogic m) => MonadKanren m where
  freshLVar :: (Data a) => m (LVar a)
  conde :: [m ()] -> m ()
  newLVar :: (Data a) => a -> m (LVar a)
  unifyLVar :: (Unifiable a) => LVar a -> LVar a -> m ()
  successful :: m ()
  unsuccessful :: m ()

  conde xs = foldr interleave mzero xs

  successful = return ()
  unsuccessful = mzero


instance (Monad m) => MonadKanren (MiniKanrenT m) where
  freshLVar = MiniKanrenT $ newUnboundLVar
  newLVar a = MiniKanrenT $ newBoundLVar a
  unifyLVar a b = do
      theSame <- MiniKanrenT $ eqLVar a b
      when (not $ theSame) $ do
        a' <- MiniKanrenT $ readLVar a
        b' <- MiniKanrenT $ readLVar b
        unifyLVar' a' b'
    where
      unifyLVar' Nothing _ = MiniKanrenT $ bindLVar a b
      unifyLVar' _ Nothing = MiniKanrenT $ bindLVar b a
      unifyLVar' (Just aVal) (Just bVal) = do
        MiniKanrenT $ bindLVar a b
        unifyValue aVal bVal


