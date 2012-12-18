{-#Language DeriveDataTypeable #-}

module Control.Monad.MiniKanren.List where

import Control.Monad
import Data.Data

import Control.Monad.LogicVarT
import Control.Monad.MiniKanren.Core

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
  logic_mempty = new $ Nil
  logic_mappend a b = joinThunk $ do 
    result <- fresh
    conde
      [ do
          a === logic_mempty
          b === result
      , do
          (t, h, t') <- fresh3
          cons h t === a
          cons h t' === result
          logic_mappend t b === t'
      ]
    return result

instance LogicFunctor List where
  logic_fmap f l = joinThunk $ do
    (result, x, xs) <- fresh3
    conde [ nil === l >> nil === result
          , do
              cons x xs === l
              cons (f x) (logic_fmap f xs) === result
          ]
    
    return result

instance LogicFoldable List where
  logic_foldr f a l = joinThunk $ do
    result <- fresh
    conde [ do
              nil === l
              result === a
          , do
              (x, xs) <- fresh2
              cons x xs === l
              f x (logic_foldr f a xs) === result
          ]
    return result

instance (Unifiable a) => Matchable (List a) where
  match f a = joinThunk $ do
    result <- fresh
    conde
      [ do
          nil === a
          f Nil === result
      , do
          (x, xs) <- fresh2
          cons x xs === a
          x' <- evalThunk x
          xs' <- evalThunk xs
          f (Cons x' xs') === result
      ]
    return result


logic_fold f a = match (logic_fold' f a)
logic_fold' f a Nil = a
logic_fold' f a (Cons x xs) = f (thunk x) (logic_fold f a (thunk xs))


reifyList :: (Unifiable b, MonadKanren m)
          => (a -> m (LogicThunk m b)) -> [a] -> m (LogicThunk m (List b))
reifyList _ [] = return $ new Nil
reifyList f (x:xs) = liftM2 cons (f x) (reifyList f xs)



--remove unifiable?
--make fresh work on a typeclass


--logic_append
--  :: (LogicMonoid a, MonadKanren m) =>
--     LogicThunk m a -> LogicThunk m a -> LogicThunk m a
--logic_append a = match ((\x y -> logic_append y x) a)
logic_append a b = match (logic_append' b) a
-- Note that the order of the arguments was swapped
logic_append' b Nil = b
logic_append' b (Cons x xs) = cons (thunk x) (logic_append (thunk xs) b)

instance Unifiable () where
  unifyValue _ _ = successful

logic_elem a xs = match (logic_elem' a) xs
logic_elem' _ Nil = unsuccessful
logic_elem' a (Cons x xs) = conde [a === (thunk x), logic_elem a (thunk xs)]

nil :: (Unifiable a, MonadKanren m) => LogicThunk m (List a)
nil = new Nil

cons :: (Unifiable a, MonadKanren m)
     => LogicThunk m a -> LogicThunk m (List a) -> LogicThunk m (List a)
cons x xs = joinThunk $ do
  x' <- evalThunk x
  xs' <- evalThunk xs
  return $ new $ Cons x' xs'



--append Nil a = a
--append (Cons x xs) b = Cons x (append xs b)

--append([X|Y],Z,[X|W]) :- append(Y,Z,W).
--class LogicFunctor f where
--  logic_fmap :: (Unifiable a, Unifiable b)
--             => (LogicThunk a -> LogicThunk b) 
--                 -> LogicThunk (f a) -> LogicThunk (f b)

--instance LogicFunctor List where
--  logic_fmap f l = joinThunk $ do
--    (result, x, xs) <- fresh'
--    cons x xs === l
--    cons (f x) (logic_fmap f xs) === result
--    return result

