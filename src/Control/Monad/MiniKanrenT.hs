
{-#Language GeneralizedNewtypeDeriving
          , DeriveDataTypeable
          , FlexibleInstances
          , TypeSynonymInstances #-}

module Control.Monad.MiniKanrenT where

import Control.Applicative

import Control.Monad
import Control.Monad.Logic
import Control.Monad.LogicVarT

import Data.Data

newtype MiniKanrenT m a = MiniKanrenT (LogicVarT (LogicT m) a)
  deriving (Monad, Applicative,
            Alternative, Functor, MonadPlus, MonadIO, MonadLogic)

instance MonadTrans MiniKanrenT where
  lift m = MiniKanrenT $ lift $ lift m

run :: (Monad m) => Int -> MiniKanrenT m a -> m [a]
run n (MiniKanrenT m) = observeManyT n (runLogicVarT m)

fresh :: (Typeable a) => MiniKanrenT m (LVar a)
fresh = MiniKanrenT $ newLVar

(===) :: (Unifiable a) => LVar a -> LVar a -> MiniKanrenT m ()
a === b = MiniKanrenT $ unifyLVar a b

conde :: (Monad m) => [MiniKanrenT m ()] -> MiniKanrenT m () -> MiniKanrenT m ()
conde xs e = ifte (msum xs) return e

condi :: (Monad m) => [MiniKanrenT m ()] -> MiniKanrenT m () -> MiniKanrenT m ()
condi xs e = ifte (foldr interleave mzero xs) return e

successful :: (Monad m) => m ()
successful = return ()

unsuccessful :: (MonadPlus m) => m ()
unsuccessful = mzero

is :: (Unifiable a) => a -> MiniKanrenT m (LVar a)
is a = MiniKanrenT $ newBoundLVar a

