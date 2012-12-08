{-#Language GeneralizedNewtypeDeriving
          , DeriveDataTypeable
          , FlexibleInstances
          , TypeSynonymInstances
          #-}

module Control.Monad.MiniKanrenT.Term where

import Control.Monad.MiniKanrenT

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
class NewTerm a where
  newTerm :: (Monad m) => a -> MiniKanrenT m (LVar Term)

instance NewTerm String where
  newTerm s = newLVar $ Symbol s

instance (NewTerm a, NewTerm b) => NewTerm (a, b) where
  newTerm (a, b) = do
    a' <- newTerm a
    b' <- newTerm b
    newLVar $ Cons a' b'

instance NewTerm () where
  newTerm () = newLVar Nil

instance NewTerm Term where
  newTerm a = newLVar a

instance NewTerm (LVar Term) where
  newTerm a = return a


-- Define fresh in terms of our type. This just tacks a type signature onto
-- freshLVar to help the type inferrer out and make the error messages clearer
fresh :: MiniKanrenT m (LVar Term)
fresh = freshLVar

(===) :: (Monad m, NewTerm a, NewTerm b) => a -> b -> MiniKanrenT m ()
a === b = do
  a' <- newTerm a
  b' <- newTerm b
  unifyLVar a' b'

