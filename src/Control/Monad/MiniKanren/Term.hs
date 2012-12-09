{-#Language GeneralizedNewtypeDeriving
          , DeriveDataTypeable
          , FlexibleInstances
          , TypeSynonymInstances
          #-}

module Control.Monad.MiniKanren.Term where

import Control.Monad.MiniKanren.Core

import Data.Data

-- A datatype for playing around with lists
data Term = Symbol String
          | Cons (LVar Term) (LVar Term)
          | Nil
  deriving (Show, Eq, Ord, Data, Typeable)

-- Define unification for our new datatype 
instance Unifiable Term where
  unifyValue (Symbol a) (Symbol b) | a == b = successful
  unifyValue Nil Nil = successful
  unifyValue (Cons a1 a2) (Cons b1 b2) = do
    a1 === b1
    a2 === b2
  unifyValue _ _ = unsuccessful

-- This class is just for convenience. It makes it much easier to construct 
--  terms and makes our unification operator (===) much more flexible
class CanBeTerm a where
  newTerm :: (MonadKanren m) => a -> m (LVar Term)

instance CanBeTerm String where
  newTerm s = newLVar $ Symbol s

instance (CanBeTerm a, CanBeTerm b) => CanBeTerm (a, b) where
  newTerm (a, b) = do
    a' <- newTerm a
    b' <- newTerm b
    newLVar $ Cons a' b'

instance CanBeTerm () where
  newTerm () = newLVar Nil

instance CanBeTerm Term where
  newTerm a = newLVar a

instance CanBeTerm (LVar Term) where
  newTerm a = return a


-- Define fresh in terms of our type. This just tacks a type signature onto
-- freshLVar to help the type inferrer out and make the error messages clearer
fresh :: (MonadKanren m) => m (LVar Term)
fresh = freshLVar

(===) :: (MonadKanren m, CanBeTerm a, CanBeTerm b) => a -> b -> m ()
a === b = do
  a' <- newTerm a
  b' <- newTerm b
  unifyLVar a' b'

