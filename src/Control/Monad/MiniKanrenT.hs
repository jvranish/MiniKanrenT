
{-#Language GeneralizedNewtypeDeriving
          #-}

module Control.Monad.MiniKanrenT 
  ( LVar, lvarKey, lvarValue
  , MiniKanren, MiniKanrenT, Unifiable(..)
  , run, runAll, runT, runAllT
  , freshLVar
  , conde, condi
  , successful, unsuccessful
  , newLVar, unifyLVar
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

type MiniKanren = MiniKanrenT Identity

runAll :: (Data a) => MiniKanren a -> [a]
runAll (MiniKanrenT m) = runIdentity $ 
  observeAllT (runLogicVarT (m >>= unrollLVars))

run :: (Data a) => Int -> MiniKanren a -> [a]
run n (MiniKanrenT m) = runIdentity $ 
  observeManyT n (runLogicVarT (m >>= unrollLVars))

runT :: (Monad m, Data a) => Int -> MiniKanrenT m a -> m [a]
runT n (MiniKanrenT m) = observeManyT n (runLogicVarT (m >>= unrollLVars))

runAllT :: (Monad m, Data a) => MiniKanrenT m a -> m [a]
runAllT (MiniKanrenT m) = observeAllT (runLogicVarT (m >>= unrollLVars))

freshLVar :: (Data a) => MiniKanrenT m (LVar a)
freshLVar = MiniKanrenT $ newUnboundLVar

conde :: (Monad m) => [MiniKanrenT m ()] -> MiniKanrenT m () -> MiniKanrenT m ()
conde xs e = msum (xs ++ [e])

condi :: (Monad m) => [MiniKanrenT m ()] -> MiniKanrenT m () -> MiniKanrenT m ()
condi xs e = foldr interleave mzero (xs ++ [e])

successful :: (Monad m) => m ()
successful = return ()

unsuccessful :: (MonadPlus m) => m ()
unsuccessful = mzero

newLVar :: (Unifiable a, Data a) => a -> MiniKanrenT m (LVar a)
newLVar a = MiniKanrenT $ newBoundLVar a

unifyLVar :: (Monad m, Unifiable a) => LVar a -> LVar a -> MiniKanrenT m ()
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


