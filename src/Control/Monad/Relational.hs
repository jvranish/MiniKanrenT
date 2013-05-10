{-#Language StandaloneDeriving
          , TypeFamilies
          , GADTs
          , GeneralizedNewtypeDeriving
          , RankNTypes
            #-}
 
 module Control.Monad.Relational where

import Control.Monad.Trans.Class

import Control.Monad.Stream
import Control.Monad.Logic hiding (msum, mapM_)

import Control.Monad.LogicVarT


import Data.Maybe
import Data.Foldable

import Prelude hiding (mapM_)


class (MonadLogicVar m) => MonadRelational m where
  fresh :: (Possibilities f) => m (m (f m))
  (===) :: (Unifiable f) => m (f m) -> m (f m) -> m ()

class Unifiable f where
  unifyValue :: (MonadRelational m) => f m -> f m -> m ()

class (Unifiable f) => Possibilities f where
  possibilities :: (MonadRelational m) => [ m (f m) ]


class Evalable f where
  eval :: (Monad m) => (forall g. Evalable g => m (g m) -> m (m1 (g m1))) -> f m -> m (f m1)



newtype Rel m a = Rel { unRel :: m (LogicValue (Rel m) a) }


data LogicValue m a where
  LogicValue :: (Possibilities f, f m ~ a) => LVar m a -> LogicValue m a -- (Possibilities f, f m ~ a) => LVar m (f m) -> LogicValue m (f m)
  LogicPureValue :: a -> LogicValue m a



instance (MonadLogicVar m, MonadLogic m) => Monad (Rel m) where
  (Rel m) >>= k = Rel $ (>>- id) $ liftM (unRel . doit k) m
    where
      doit :: (MonadLogic m, MonadLogicVar m) => (a -> Rel m b) -> LogicValue (Rel m) a -> Rel m b
      doit k' (LogicValue var) = match (Rel $ return $ LogicValue var) k'
      doit k' (LogicPureValue x) = k' x
  return a = Rel $ return $ LogicPureValue a


match :: (Possibilities f, MonadLogic m, MonadLogicVar m) =>
         Rel m (f (Rel m)) -> ((f (Rel m)) -> Rel m b) -> Rel m b
match a f = msum [ x === a >> (x >>= f) | x <- possibilities ]


instance (MonadLogic m, MonadLogicVar m) => MonadPlus (Rel m) where
  mzero = Rel $ mzero
  mplus (Rel m1) (Rel m2) = Rel $ interleave m1 m2 

instance MonadTrans Rel where
  lift m = Rel $ liftM LogicPureValue m


instance (MonadLogicVar m, MonadLogic m, Monad m) => MonadRelational (Rel m) where
  fresh = do
    a <- newLVar Nothing
    return (Rel (return $ LogicValue a))

  (Rel a) === (Rel b) = do
      a' <- lift a >>= logicValue
      b' <- lift b >>= logicValue
      unifyLVar (unifyValue) a' b'
    where
      logicValue :: (MonadLogicVar m) => LogicValue m a -> m (LVar m a)
      logicValue (LogicValue x) = return x
      logicValue (LogicPureValue x) = newLVar (Just x)


instance (MonadLogic m, MonadLogicVar m) => MonadLogicVar (Rel m) where
  data LVar (Rel m) a = RelLVar (LVar m a)
  newLVar a = liftM RelLVar $ lift $ newLVar a
  readLVar (RelLVar a) = lift $ readLVar a
  sameLVar (RelLVar a) (RelLVar b) = lift $ sameLVar a b
  substLVar (RelLVar a) (RelLVar b) = lift $ substLVar a b
  keyLVar (RelLVar a) = lift $ keyLVar a



enumerate :: (Evalable f, Monad m, Monad m1) => m (f m) -> m (m1 (f m1))
enumerate x = x >>= (liftM return . eval enumerate)

unReify :: (Evalable f, MonadLogicVar m, MonadLogic m) => Rel m (f (Rel m)) -> Rel m (LExpr (f LExpr))
unReify (Rel m) = do
  a <- Rel $ liftM LogicPureValue m
  case a of
    LogicValue var -> do
      x <- readLVar var
      key <- keyLVar var
      case x of
        Nothing -> return $ Unbound key
        Just y -> do
          a' <- eval unReify y
          return $ Bound key a'
    LogicPureValue x -> do
      a' <- eval unReify x
      return $ Pure a'

runRelation :: (Evalable f) => (forall m. (MonadLogicVar m, MonadLogic m) => Rel m (f (Rel m))) -> [LExpr (f LExpr)]
runRelation m = runIt $ unReify m
  where
    runIt :: (Evalable f) => (forall s. Rel (LogicVarT s Stream) (LExpr (f LExpr))) -> [LExpr (f LExpr)]
    runIt m' = toList $ runLogicVarT (liftM unPure $ unRel m')

    unPure (LogicPureValue a) = a
    unPure _ = error "I know, I know, this is unforgivable, but hard reasons! I promise I fix :("

data LExpr a = Unbound Int | Bound Int a | Pure a

instance (Show a) => Show (LExpr a) where
  showsPrec p (Unbound n) = showsPrec p n
  showsPrec p (Bound _ a) = showsPrec p a
  showsPrec p (Pure a) = showsPrec p a







