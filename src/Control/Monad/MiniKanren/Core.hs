
{-#Language GeneralizedNewtypeDeriving
          -- #TODO remove this
          , DeriveDataTypeable
          -- , FlexibleInstances
          , RankNTypes
          #-}

module Control.Monad.MiniKanren.Core 
  ( LVar, lvarKey, lvarValue
  , MiniKanren, MiniKanrenT, UnifiableValue(..)
  , run, runAll, runT, runAllT
  , MonadKanren(..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Logic
import Control.Monad.LogicVarT

import Data.Data
import Data.Functor.Identity

import Data.Monoid

class (Data a) => UnifiableValue a where
  unifyValue :: (MonadLogicVar m) => a -> a -> m ()


class Freshable a where
  fresh' :: (MonadKanren m) => m a

instance (UnifiableValue a) => Freshable (LogicThunk a) where
  fresh' = do
    lvar <- freshLVar
    return (LogicThunk $ return lvar)

instance (Freshable a, Freshable b) => Freshable (a, b) where
  fresh' = liftM2 (,) fresh' fresh'

instance (Freshable a, Freshable b, Freshable c) => Freshable (a, b, c) where
  fresh' = liftM3 (,,) fresh' fresh' fresh'
  
instance (Freshable a, Freshable b, Freshable c, Freshable d) => Freshable (a, b, c, d) where
  fresh' = liftM4 (,,,) fresh' fresh' fresh' fresh'

class Unifiable a where
  fresh :: (MonadKanren m) => m a
  (===) :: (MonadKanren m) => a -> a -> m ()

newtype MiniKanrenT m a = MiniKanrenT (LogicVarT (LogicT m) a)
  deriving (Monad, Applicative, Alternative, Functor, MonadPlus,
            MonadIO, MonadLogic, MonadLogicVar)

instance MonadTrans MiniKanrenT where
  lift m = MiniKanrenT $ lift $ lift m


newtype MiniKanren a = MiniKanren (MiniKanrenT Identity a)
  deriving (Monad, Applicative, Alternative, Functor, MonadPlus,
            MonadLogic, MonadLogicVar, MonadKanren)


runAll :: (Data a) => MiniKanren a -> [a]
runAll (MiniKanren m) = runIdentity $ runAllT m

run :: (Data a) => Int -> MiniKanren a -> [a]
run n (MiniKanren m) = runIdentity $ runT n m

runT :: (Monad m, Data a) => Int -> MiniKanrenT m a -> m [a]
runT n (MiniKanrenT m) = observeManyT n (runLogicVarT (m >>= unrollLVars))

runAllT :: (Monad m, Data a) => MiniKanrenT m a -> m [a]
runAllT (MiniKanrenT m) = observeAllT (runLogicVarT (m >>= unrollLVars))


class (MonadPlus m) => MonadLogicVar m where
  freshLVar :: (Data a) => m (LVar a)
  newLVar :: (Data a) => a -> m (LVar a)
  unifyLVar :: (UnifiableValue a) => LVar a -> LVar a -> m ()

class (MonadLogic m, MonadLogicVar m) => MonadKanren m where

conde :: (MonadLogic m) => [m ()] -> m ()
conde xs = foldr interleave mzero xs

successful :: (Monad m) => m ()
successful = return ()

unsuccessful :: (MonadPlus m) => m ()
unsuccessful = mzero

-- #TODO might not need these
instance (UnifiableValue a) => Unifiable (LVar a) where
  fresh = freshLVar
  a === b = unifyLVar a b

instance (UnifiableValue a) => Unifiable (LogicThunk a) where
  fresh = do
    lvar <- freshLVar
    return (LogicThunk $ return lvar)
  a === b = do
    a' <- evalThunk a
    b' <- evalThunk b
    unifyLVar a' b'

instance (MonadPlus m) => MonadLogicVar (LogicVarT m) where
  freshLVar = newUnboundLVar
  newLVar a = newBoundLVar a
  unifyLVar a b = do
      theSame <- eqLVar a b
      when (not $ theSame) $ do
        a' <- readLVar a
        b' <- readLVar b
        unifyLVar' a' b'
    where
      unifyLVar' Nothing _ = bindLVar a b
      unifyLVar' _ Nothing = bindLVar b a
      unifyLVar' (Just aVal) (Just bVal) = do
        bindLVar a b
        unifyValue aVal bVal


instance (Monad m) => MonadKanren (MiniKanrenT m) where



data LogicThunk a = LogicThunk { evalThunk :: forall m . MonadKanren m => (m (LVar a)) }




-- #TODO do I really want this?
new :: (UnifiableValue a) => a -> LogicThunk a 
new a = LogicThunk $ do
  lvar <- newLVar a
  return lvar


data List a = Cons (LVar a) (LVar (List a))
            | Nil
  deriving (Show, Eq, Ord, Data, Typeable)

instance (UnifiableValue a) => UnifiableValue (List a) where
  unifyValue Nil Nil = successful
  unifyValue (Cons a1 a2) (Cons b1 b2) = do
    unifyLVar a1 b1
    unifyLVar a2 b2
  unifyValue _ _ = unsuccessful

data Symbol = Symbol String
  deriving (Show, Eq, Ord, Data, Typeable)

instance UnifiableValue Symbol where
  unifyValue (Symbol a) (Symbol b) | a == b = successful
  unifyValue _ _ = unsuccessful

reifySymbol :: String -> LogicThunk Symbol
reifySymbol s = new $ Symbol s


reifyList :: UnifiableValue b
          => (a -> LogicThunk b) -> [a] -> LogicThunk (List b)
reifyList _ [] = new Nil
reifyList f (x:xs) = cons (f x) (reifyList f xs)


class LogicMonoid a where
  logic_mempty :: LogicThunk a
  logic_mappend :: LogicThunk a -> LogicThunk a -> LogicThunk a 

--remove unifiable?
--make fresh work on a typeclass

instance (UnifiableValue a) => LogicMonoid (List a) where
  logic_mempty = new $ Nil
  logic_mappend a b = joinThunk $ do 
    result <- fresh
    conde
      [ do
          a === logic_mempty
          b === result
      , do
          (t, h, t') <- fresh'
          cons h t === a
          cons h t' === result
          logic_mappend t b === t'
      ]
    return result

class LogicFunctor f where
  logic_fmap :: (UnifiableValue a, UnifiableValue b)
             => (LogicThunk a -> LogicThunk b) 
                 -> LogicThunk (f a) -> LogicThunk (f b)

instance LogicFunctor List where
  logic_fmap f l = joinThunk $ do
    (result, x, xs) <- fresh'
    cons x xs === l
    cons (f x) (logic_fmap f xs) === result
    return result


instance LogicMonoid a => Monoid (LogicThunk a) where
  mempty = logic_mempty
  mappend = logic_mappend


test = run 5 $ do
  let a = reifyList reifySymbol ["1", "2", "3"]
  --let b = reifyList reifySymbol ["4", "5", "6"]
  let c = reifyList reifySymbol ["1", "2", "3", "4", "5", "6"]
  b <- fresh

  --c <- evalThunk $ (a `mappend` b)
  c === (a `mappend` b)
  b' <- evalThunk b
  return b'

joinThunk :: (forall m. (MonadKanren m) => m (LogicThunk a)) -> LogicThunk a 
joinThunk thunk = LogicThunk $ join $ liftM evalThunk $ thunk
-- 

--liftThunk
--liftThunk2

cons :: (UnifiableValue a)
     => LogicThunk a -> LogicThunk (List a) -> LogicThunk (List a)
cons x xs = joinThunk $ do
  x' <- evalThunk x
  xs' <- evalThunk xs
  return $ new $ Cons x' xs'


