
{-#Language GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Control.Monad.MiniKanren.Core 
  ( LVar, lvarKey, lvarValue
  , MiniKanren, MiniKanrenT, Unifiable(..)
  , fresh2, fresh3, fresh4
  , run, runAll, runT, runAllT
  , MonadKanren(..), Thunk
  , Matchable(..)
  , new, thunk
  , LogicFunctor(..), LogicFoldable(..)
  , LogicMonoid(..)
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Logic
import Control.Monad.LogicVarT

import Data.Data
import Data.Functor.Identity

import Data.Monoid

import Data.String


class (MonadLogic m, MonadLogicVar m) => MonadKanren m where

  conde :: [m ()] -> m ()
  fresh :: (Unifiable a) => m (Thunk m a)
  (===) :: (Unifiable a) => Thunk m a -> Thunk m a -> m ()

  conde xs = foldr interleave mzero xs

  fresh = do
    a <- freshLVar
    return $ return a

  a === b = bindM2 unifyLVar a b


newtype MiniKanrenT m a = MiniKanrenT (LogicVarT (LogicT m) a)
  deriving (Monad, Applicative, Alternative, Functor, MonadPlus,
            MonadIO, MonadLogic, MonadLogicVar)

instance MonadTrans MiniKanrenT where
  lift m = MiniKanrenT $ lift $ lift m

instance (Monad m) => MonadKanren (MiniKanrenT m) where

newtype MiniKanren a = MiniKanren (MiniKanrenT Identity a)
  deriving (Monad, Applicative, Alternative, Functor, MonadPlus,
            MonadLogic, MonadLogicVar, MonadKanren, IsString) -- Num, Monoid


run :: (Data a) => Int -> MiniKanren a -> [a]
run n (MiniKanren m) = runIdentity $ runT n m

runAll :: (Data a) => MiniKanren a -> [a]
runAll (MiniKanren m) = runIdentity $ runAllT m

runT :: (Monad m, Data a) => Int -> MiniKanrenT m a -> m [a]
runT n (MiniKanrenT m) = observeManyT n (runLogicVarT (m >>= unrollLVars))

runAllT :: (Monad m, Data a) => MiniKanrenT m a -> m [a]
runAllT (MiniKanrenT m) = observeAllT (runLogicVarT (m >>= unrollLVars))


fresh2 :: (Unifiable a, Unifiable b, MonadKanren m)
       => m (Thunk m a, Thunk m b)
fresh2 = liftM2 (,) fresh fresh

fresh3 :: (Unifiable a, Unifiable b, Unifiable c, MonadKanren m)
       => m (Thunk m a, Thunk m b, Thunk m c)
fresh3 = liftM3 (,,) fresh fresh fresh

fresh4 :: (Unifiable a, Unifiable b, Unifiable c, Unifiable d, MonadKanren m)
       => m (Thunk m a, Thunk m b, Thunk m c, Thunk m d)
fresh4 = liftM4 (,,,) fresh fresh fresh fresh

-- #TODO leave note on p-adic numbers 
-- #TODO can we create a structure where the variables are holes?

class LogicIsStringVar a where
  fromStringLVar :: (MonadKanren m) => String -> m a

instance (Unifiable a, IsString a) => LogicIsStringVar (LVar a) where
  fromStringLVar s = new $ fromString s

type Thunk m a = m (LVar a)
-- add monoid and isString
instance (LogicIsStringVar a, Monad m) => IsString (MiniKanrenT m a) where
  fromString = fromStringLVar


--class LogicMonoidVar a where
--  memptyLVar :: (MonadKanren m) => m a  
--  mappendLVar :: (MonadKanren m) => a -> a -> m a

--instance (LogicMonoid a) => LogicMonoidVar (LVar a) where
--  memptyLVar = logic_mempty
--  mappendLVar = logic_mappend

instance (LogicMonoid a, MonadKanren m) => Monoid (Thunk m a) where
  mempty = logic_mempty
  mappend = logic_mappend

new :: (Unifiable a, MonadKanren m) => a -> Thunk m a
new a = newLVar a

-- #TODO remove?
thunk :: (MonadKanren m) => LVar a -> Thunk m a
thunk = return 

class (Unifiable a) => Matchable a where
  match :: (MonadKanren m, Unifiable b) => (a -> Thunk m b) -> Thunk m a -> Thunk m b
  match_ :: (MonadKanren m) => (a -> m b) -> Thunk m a -> m ()

  match_ f t = do
    result <- fresh
    -- #TODO clean this up
    _ <- match (const (result `asTypeOf` t) . f) t
    return ()

class LogicMonoid a where
  logic_mempty  :: (MonadKanren m) => Thunk m a
  logic_mappend :: (MonadKanren m) => Thunk m a -> Thunk m a -> Thunk m a 

class LogicFunctor f where
  logic_fmap :: (Unifiable a, Unifiable b, MonadKanren m)
             => (Thunk m a -> Thunk m b) -> Thunk m (f a) -> Thunk m (f b)

class LogicFoldable f where
  logic_foldr :: (Unifiable a, Unifiable b, MonadKanren m)
              => (Thunk m a -> Thunk m b -> Thunk m b) 
                 -> Thunk m b -> Thunk m (f a) -> Thunk m b


class (Unifiable a) => LogicNum a where
  logic_add :: (MonadKanren m)
             => Thunk m a -> Thunk m a -> Thunk m a

--class LogicNumVar a where
--  logic_addLVar :: MonadKanren m => a -> a -> m a

--instance (LogicNum a) => LogicNumVar (LVar a) where
--  logic_addLVar = logic_add

--instance (Monad m, LogicNumVar a) => Num (MiniKanrenT m a) where
--  a + b = bindM2 logic_addLVar a b

instance (MonadKanren m, LogicNum a) => Num (Thunk m a) where
  a + b = logic_add a b


--instance (Monad m, LogicNumVar a) => Num (MiniKanrenT Thunk m a) where
--  a + b = bindM2 logic_add a b

bindM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do
  a' <- a
  b' <- b
  f a' b'

boo :: (MonadKanren m) => Thunk m a
boo = undefined


--test = run 5 $ do
--  a <- reifyList reifySymbol ["1", "2", "3"]
--  --let b = reifyList reifySymbol ["4", "5", "6"]
--  -- let c = reifyList reifySymbol ["1", "2", "3", "4", "5", "6"]
--  c <- fresh
--  b <- fresh

--  --c <- evalThunk $ (a `mappend` b)
--  c === (b `mappend` a)
--  evalThunk c



