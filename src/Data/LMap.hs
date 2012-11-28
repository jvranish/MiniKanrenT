{-#Language DeriveDataTypeable
          #-}
module Data.LMap where

import Control.Monad
import Data.Data

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Data.Dynamic

import Data.Maybe

newtype Key = Key IntMap.Key
  deriving (Show, Eq, Ord, Data, Typeable)

newtype LMap = LMap (IntMap.IntMap (Dynamic, IntSet.IntSet))

empty :: LMap
empty = LMap IntMap.empty

fromSet :: (Key -> a) -> IntSet.IntSet -> IntMap.IntMap a
fromSet f set = IntSet.foldr 
  (\k t -> IntMap.insert k (f (Key k)) t) IntMap.empty set

-- This should not be used externally
insertMany :: IntSet.IntSet -> Dynamic -> LMap -> LMap
insertMany set v (LMap t) = LMap $ 
  IntMap.union (fromSet (const (v, set)) set) t

insert :: (Typeable a) => Key -> a -> LMap -> LMap
insert (Key k) v (LMap t) = case IntMap.lookup k t of
  Just (_, set) -> insertMany set (toDyn v) (LMap t)
  Nothing       -> LMap $ IntMap.insert k (toDyn v, IntSet.singleton k) t

lookup :: (Typeable a) => Key -> LMap -> Maybe a
lookup (Key k) (LMap t) = fromDynamic =<< return . fst =<< IntMap.lookup k t

bind :: Key -> Key -> LMap -> LMap
bind (Key this) (Key to_that) t | this == to_that = t
bind (Key this) (Key to_that) (LMap t) = fromMaybe (LMap t) $ do
  (_, this_set) <- IntMap.lookup this t
  -- This guard will bail us out if these keys are already bound together
  guard $ not $ IntSet.member to_that this_set 
  (v, that_set) <- IntMap.lookup to_that t
  return $ insertMany (IntSet.union this_set that_set) v (LMap t)

eqKey :: Key -> Key -> LMap -> Bool
eqKey (Key a) (Key b) _ | a == b = True 
eqKey (Key a) (Key b) (LMap t) = case IntMap.lookup a t of
  Just (_, set) -> IntSet.member b set
  Nothing -> False

