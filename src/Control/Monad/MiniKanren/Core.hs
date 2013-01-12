
{-#Language GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Control.Monad.MiniKanren.Core 
  ( LVar, lvarKey, lvarValue
  , MiniKanren, MiniKanrenT, Unifiable(..)
  , fresh2, fresh3, fresh4
  , run, runAll, runT, runAllT
  , MonadKanren(..)
  , Matchable(..)
  , new, thunk, LogicThunk(..)
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
  fresh :: (Unifiable a) => m (LogicThunk m a)
  (===) :: (Unifiable a) => LogicThunk m a -> LogicThunk m a -> m ()

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
       => m (LogicThunk m a, LogicThunk m b)
fresh2 = liftM2 (,) fresh fresh

fresh3 :: (Unifiable a, Unifiable b, Unifiable c, MonadKanren m)
       => m (LogicThunk m a, LogicThunk m b, LogicThunk m c)
fresh3 = liftM3 (,,) fresh fresh fresh

fresh4 :: (Unifiable a, Unifiable b, Unifiable c, Unifiable d, MonadKanren m)
       => m (LogicThunk m a, LogicThunk m b, LogicThunk m c, LogicThunk m d)
fresh4 = liftM4 (,,,) fresh fresh fresh fresh

-- #TODO leave note on p-adic numbers 
-- #TODO can we create a structure where the variables are holes?

class LogicIsStringVar a where
  fromStringLVar :: (MonadKanren m) => String -> m a

instance (Unifiable a, IsString a) => LogicIsStringVar (LVar a) where
  fromStringLVar s = new $ fromString s

type LogicThunk m a = m (LVar a)
-- add monoid and isString
instance (LogicIsStringVar a, Monad m) => IsString (MiniKanrenT m a) where
  fromString = fromStringLVar


--class LogicMonoidVar a where
--  memptyLVar :: (MonadKanren m) => m a  
--  mappendLVar :: (MonadKanren m) => a -> a -> m a

--instance (LogicMonoid a) => LogicMonoidVar (LVar a) where
--  memptyLVar = logic_mempty
--  mappendLVar = logic_mappend

instance (LogicMonoid a, MonadKanren m) => Monoid (LogicThunk m a) where
  mempty = logic_mempty
  mappend = bindM2 logic_mappend

new :: (Unifiable a, MonadKanren m) => a -> LogicThunk m a
new a = newLVar a

-- #TODO remove?
thunk :: (MonadKanren m) => LVar a -> LogicThunk m a
thunk = return 

--joinThunk :: (MonadKanren m) => m (LogicThunk m a) -> LogicThunk m a 
--joinThunk t = LogicThunk $ join $ liftM evalThunk t


--liftThunk
--liftThunk2



class (Unifiable a) => Matchable a where
  match :: (MonadKanren m, Unifiable b) => (a -> LogicThunk m b) -> LogicThunk m a -> LogicThunk m b
  match_ :: (MonadKanren m) => (a -> m b) -> LogicThunk m a -> m ()

  match_ f t = do
    result <- fresh
    -- #TODO clean this up
    _ <- match (const (result `asTypeOf` t) . f) t
    return ()

class LogicMonoid a where
  logic_mempty :: (MonadKanren m) => LogicThunk m a
  logic_mappend :: (MonadKanren m) => LVar a -> LVar a -> LogicThunk m a 


class LogicFunctor f where
  logic_fmap :: (Unifiable a, Unifiable b, MonadKanren m)
             => (LogicThunk m a -> LogicThunk m b) 
                 -> LogicThunk m (f a) -> LogicThunk m (f b)

class LogicFoldable f where
  logic_foldr :: (Unifiable a, Unifiable b, MonadKanren m)
              => (LogicThunk m a -> LogicThunk m b -> LogicThunk m b) 
                  -> LogicThunk m b -> LogicThunk m (f a) -> LogicThunk m b


class (Unifiable a) => LogicNum a where
  logic_add :: (MonadKanren m)
             => LVar a -> LVar a -> m (LVar a) --LogicThunk m a

--class LogicNumVar a where
--  logic_addLVar :: MonadKanren m => a -> a -> m a

--instance (LogicNum a) => LogicNumVar (LVar a) where
--  logic_addLVar = logic_add

--instance (Monad m, LogicNumVar a) => Num (MiniKanrenT m a) where
--  a + b = bindM2 logic_addLVar a b

instance (MonadKanren m, LogicNum a) => Num (LogicThunk m a) where
  a + b = bindM2 logic_add a b

--can I make any instance of monadkanren automaticall be a num instance?
--class (MonadLogic m, MonadLogicVar m) => MonadKanren m where

--  conde :: [m ()] -> m ()
--  fresh :: (Unifiable a) => m (LogicThunk m a)
--  (===) :: (Unifiable a) => LogicThunk m a -> LogicThunk m a -> m ()
--instance (Monad m, LogicNumVar a) => Num (MiniKanrenT m (LVar a)) where
--  a + b = bindM2 logic_add a b

bindM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do
  a' <- a
  b' <- b
  f a' b'

boo :: (MonadKanren m) => LogicThunk m a
boo = undefined


-- MiniKanrenT m (LVar a)
--class FOO a where
--  foo :: f a -> LogicThunk m ( b)
--  LVar a 

--instance Num (MiniKanrenT m a) where
--  a + b = logic_add a b

--test = run 5 $ do
--  a <- reifyList reifySymbol ["1", "2", "3"]
--  --let b = reifyList reifySymbol ["4", "5", "6"]
--  -- let c = reifyList reifySymbol ["1", "2", "3", "4", "5", "6"]
--  c <- fresh
--  b <- fresh

--  --c <- evalThunk $ (a `mappend` b)
--  c === (b `mappend` a)
--  evalThunk c



