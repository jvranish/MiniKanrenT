{-#Language DeriveDataTypeable, DefaultSignatures #-}

module Control.Monad.MiniKanren.List where

import Control.Monad
import Data.Data

import Control.Monad.LogicVarT
import Control.Monad.MiniKanren.Core
import Control.Monad.MiniKanren.Tuple

import Data.Generics.Text

data List a = Cons (LVar a) (LVar (List a))
            | Nil
  deriving (Show, Eq, Ord, Data, Typeable)

instance (Unifiable a) => Unifiable (List a) where
  unifyValue Nil Nil = successful
  unifyValue (Cons a1 a2) (Cons b1 b2) = do
    unifyLVar a1 b1
    unifyLVar a2 b2
  unifyValue _ _ = unsuccessful

instance (Unifiable a) => LogicMonoid (List a) where
  logic_mempty = nil
  logic_mappend a b = match (logic_append' b) (return a)
    where
      -- Note that the order of the arguments was swapped
      logic_append' b Nil = return b
      logic_append' b (Cons x xs) = cons (thunk x) (logic_mappend xs b)

instance LogicFunctor List where
  logic_fmap f l = match (logic_fmap' f) l
    where
      logic_fmap' f Nil = nil
      logic_fmap' f (Cons x xs) = cons (f $ thunk x) (logic_fmap f (thunk xs))

instance LogicFoldable List where
  logic_foldr f a l = match (logic_foldr' f a) l
    where
      logic_foldr' f a Nil = a
      logic_foldr' f a (Cons x xs) = f (thunk x) (logic_foldr f a (thunk xs))

instance (Unifiable a) => Matchable (List a) where
  match f a = do
    result <- fresh
    conde
      [ do
          nil === a
          f Nil === result
      , do
          (x, xs) <- fresh2
          cons x xs === a
          x' <- x
          xs' <- xs
          f (Cons x' xs') === result
      ]
    result


--m a -> (a -> m b) -> m b
--match, and, or, sequence
--how do I take an lvar and extract the value out of it?


--class Foo f where
--  bla :: f a -> f String
--  default bla :: (Functor f, Data a) => f a -> f String
--  bla a = fmap gshow a


--instance Foo Maybe where


reifyList :: (Unifiable b, MonadKanren m)
          => (a -> m (LogicThunk m b)) -> [a] -> m (LogicThunk m (List b))
reifyList _ [] = return $ new Nil
reifyList f (x:xs) = liftM2 cons (f x) (reifyList f xs)

logic_lookup :: (Unifiable k, Unifiable v, MonadKanren m)
             => LogicThunk m k -> LogicThunk m (List (Tuple k v)) -> LogicThunk m v
logic_lookup k xs = match (logic_lookup' k) xs
  where
    logic_lookup' k Nil = unsuccessful
    logic_lookup' k (Cons x xs) = do
      v <- fresh
      conde [thunk x === tuple k v, v === logic_lookup k (thunk xs)]
      v

logic_elem :: (Unifiable a, MonadKanren m)
           => LogicThunk m a -> LogicThunk m (List a) -> m ()
logic_elem a xs = match_ (logic_elem' a) xs
logic_elem' _ Nil = unsuccessful
logic_elem' a (Cons x xs) = conde [a === thunk x, logic_elem a (thunk xs)]

nil :: (Unifiable a, MonadKanren m) => LogicThunk m (List a)
nil = new Nil

cons :: (Unifiable a, MonadKanren m)
     => LogicThunk m a -> LogicThunk m (List a) -> LogicThunk m (List a)
cons x xs = do
  x' <- x
  xs' <- xs
  new $ Cons x' xs'



