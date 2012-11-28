{-#Language GeneralizedNewtypeDeriving
          , DeriveDataTypeable #-}
module Control.Monad.LogicVarT where

import Control.Applicative

import Control.Monad
import Control.Monad.Logic
import Control.Monad.Trans.State

import Data.Data
import Data.Maybe

import qualified Data.LMap as LMap

class (Typeable a) => Unifiable a where
  unify :: (MonadPlus m) => a -> a -> LogicVarT m ()

newtype LVar a = LVar LMap.Key
  deriving (Show, Data, Typeable)

data LVarData = LVarData [LMap.Key] LMap.LMap

newtype LogicVarT m a = LogicVarT (StateT LVarData m a)
  deriving (Monad, MonadTrans, Applicative,
            Alternative, Functor, MonadPlus, MonadIO, MonadLogic)

runLogicVarT :: (Monad m) => LogicVarT m a -> m a
runLogicVarT (LogicVarT m) = evalStateT m (LVarData keys LMap.empty)
  where
    keys = fmap LMap.Key [0..]


newLVar :: (Monad m, Typeable a) => LogicVarT m (LVar a)
newLVar = writeLVar Nothing
  where
    writeLVar :: (Monad m, Typeable a) => Maybe a -> LogicVarT m (LVar a)
    writeLVar a = LogicVarT $ StateT $ 
      \(LVarData (key:next_keys) t) -> 
        return (LVar key, LVarData next_keys $ LMap.insert key a t)

newBoundLVar :: (Monad m, Typeable a) => a -> LogicVarT m (LVar a)
newBoundLVar a = LogicVarT $ StateT $ 
  \(LVarData (key:next_keys) t) -> 
    return (LVar key, LVarData next_keys $ LMap.insert key (Just a) t)

readLVar :: (Monad m, Typeable a) => LVar a -> LogicVarT m (Maybe a)
readLVar (LVar key) = LogicVarT $ gets $ \(LVarData _ t) -> 
  fromJust $ LMap.lookup key t

unifyLVar :: (MonadPlus m, Unifiable a) => LVar a -> LVar a -> LogicVarT m ()
unifyLVar a b = do
    theSame <- eqLVar a b
    when (not $ theSame) $ do
      a' <- readLVar a
      b' <- readLVar b
      unifyLVar' a' b'
  where
    unifyLVar' Nothing _ = a `subsLVar` b
    unifyLVar' _ Nothing = b `subsLVar` a
    unifyLVar' (Just aVal) (Just bVal) = do
      a `subsLVar` b
      unify aVal bVal

    eqLVar :: (Monad m) => LVar a -> LVar a -> LogicVarT m Bool
    eqLVar (LVar x) (LVar y) | x == y = return True
    eqLVar (LVar x) (LVar y) = LogicVarT $ gets $ \(LVarData _ t) -> LMap.eqKey x y t

    subsLVar :: (Monad m, Typeable a) => LVar a -> LVar a -> LogicVarT m ()
    subsLVar (LVar x) (LVar y) = LogicVarT $ modify $ 
      \(LVarData pool t) -> LVarData pool $ LMap.bind x y t

