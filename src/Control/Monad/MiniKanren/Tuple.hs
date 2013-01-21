

{-#Language DeriveDataTypeable #-}

module Control.Monad.MiniKanren.Tuple where

import Control.Monad
import Data.Data

import Control.Monad.LogicVarT
import Control.Monad.MiniKanren.Core

data Tuple a b = Tuple (LVar a) (LVar b)
  deriving (Show, Eq, Ord, Data, Typeable)

instance (Unifiable a, Unifiable b) => Unifiable (Tuple a b) where
  unifyValue (Tuple a1 a2) (Tuple b1 b2) = do
    unifyLVar a1 b1
    unifyLVar a2 b2

instance (Unifiable a, Unifiable b) => Matchable (Tuple a b) where
  match f x = do
    (result, a, b) <- fresh3
    tuple a b === x
    a' <- a
    b' <- b
    f (Tuple a' b') === result
    result

reifyTuple :: (Unifiable a, Unifiable b, MonadKanren m) => (m (LVar a), m (LVar b)) -> m (LVar (Tuple a b))
reifyTuple (a, b) = tuple a b

tuple :: (Unifiable a, Unifiable b, MonadKanren m)
     => m (LVar a) -> m (LVar b) -> m (LVar (Tuple a b))
tuple a b = new =<< liftM2 Tuple a b



