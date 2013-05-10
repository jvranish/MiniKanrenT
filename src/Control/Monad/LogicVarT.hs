{-#Language TypeFamilies
          , GeneralizedNewtypeDeriving
          , RankNTypes
            #-}
 
module Control.Monad.LogicVarT 
  ( MonadLogicVar(..)
  , runLogicVarT
  , LogicVarT
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Reader  hiding (msum, mapM_)
import Control.Monad.Logic hiding (msum, mapM_)

import Control.Monad.PureRefT

import Control.Applicative

import Data.Maybe

import qualified Data.IntMap as IntMap

import Prelude hiding (mapM_)

class (MonadPlus m) => MonadLogicVar m where
  data LVar m :: * -> *
  newLVar :: Maybe a -> m (LVar m a)
  sameLVar :: LVar m a -> LVar m a -> m Bool
  substLVar :: LVar m a -> LVar m a -> m ()
  readLVar :: (Monad m) => LVar m a -> m (Maybe a)
  keyLVar :: (Monad m) => LVar m a -> m Int

  unifyLVar :: (a -> a -> m ()) -> LVar m a -> LVar m a -> m ()
  unifyLVar f a b = do
      same <- sameLVar a b
      when (not same) $ do 
        a' <- readLVar a
        b' <- readLVar b
        unifyLVar' a' b'
    where
      unifyLVar' Nothing _ = substLVar a b
      unifyLVar' _ Nothing = substLVar b a
      unifyLVar' (Just aVal) (Just bVal) = do
        substLVar a b
        f aVal bVal


newtype LogicVarT s m a = LogicVarT { unLogicVarT :: PureRefT s m a }
  deriving (Monad, MonadTrans, Applicative,
            Alternative, Functor, MonadPlus, MonadIO, MonadLogic)

runLogicVarT :: (Monad m) => (forall s. LogicVarT s m a) -> m a
runLogicVarT m = runPureRefT $ unLogicVarT m


instance (MonadPlus m) => MonadLogicVar (LogicVarT s m) where
  data LVar (LogicVarT s m) a = LVar (Ref s (Maybe a, IntMap.IntMap (LVar (LogicVarT s m) a)))
  newLVar a = LogicVarT $ do
    ref <- newRef $ (a, IntMap.empty)
    writeRef ref (a, IntMap.singleton (refKey ref) (LVar ref))
    return $ LVar ref
  substLVar (LVar a) (LVar b) = do
        (_ , a_set) <- LogicVarT $ readRef a
        (b', b_set) <- LogicVarT $ readRef b
        when (not $ IntMap.member (refKey a) b_set) $ do
          let s = IntMap.union a_set b_set
          LogicVarT $ bulkWriteRef (fmap (\(LVar r) -> r) $ IntMap.elems s) (b', s)

  readLVar (LVar ref) = LogicVarT $ liftM fst $ readRef ref 

  sameLVar (LVar a) (LVar b) = do
        (_, a_set) <- LogicVarT $ readRef a
        return $ IntMap.member (refKey b) a_set

  keyLVar (LVar ref) = do
        (_, s) <- LogicVarT $ readRef ref
        case IntMap.keys s of
          (x:_) -> return x
          [] -> return (refKey ref)
   
