{-#Language GeneralizedNewtypeDeriving
          , DeriveDataTypeable #-}
module Control.Monad.LogicVarT 
  ( LogicVarT
  , Unifiable(..)
  , MonadLogicVar(..)
  , LVar, lvarKey, lvarValue
  , runLogicVarT
  , newUnboundLVar, newBoundLVar
  , readLVar, bindLVar, eqLVar
  , unrollLVars
  , )
where

import Control.Applicative

import Control.Monad
import Control.Monad.Logic
import Control.Monad.Trans.State

import Data.Data
import Data.Maybe
import Data.Dynamic

import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

import Data.Generics.Aliases hiding (GT)

class (Data a) => Unifiable a where
  unifyValue :: (MonadLogicVar m) => a -> a -> m ()

class (MonadPlus m) => MonadLogicVar m where
  freshLVar :: (Data a) => m (LVar a)
  newLVar :: (Data a) => a -> m (LVar a)
  unifyLVar :: (Unifiable a) => LVar a -> LVar a -> m ()
  --successful :: (Monad m) => m ()
  successful :: m ()
  successful = return ()

  --unsuccessful :: (MonadPlus m) => m ()
  unsuccessful :: m a
  unsuccessful = mzero

instance Unifiable () where
  unifyValue _ _ = successful

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

data LVar a = LVar { lvarKey :: IntMap.Key, lvarValue :: Maybe a }
  deriving (Data, Typeable)

instance (Eq a) => Eq (LVar a) where
  (LVar a Nothing) == (LVar b Nothing) = a == b 
  (LVar _ (Just a)) == (LVar _ (Just b)) = a == b 
  _ == _ = False

instance (Ord a) => Ord (LVar a) where
  compare (LVar a Nothing) (LVar b Nothing) = compare a b
  compare (LVar _ (Just a)) (LVar _ (Just b)) = compare a b
  compare (LVar _ (Just _)) (LVar _ Nothing) = GT
  compare (LVar _ Nothing) (LVar _ (Just _)) = LT

instance (Data a, Show a) => Show (LVar a) where
  showsPrec n (LVar k a') = case a' of
      Nothing -> showString varName
      Just a | reachable k IntSet.empty a -> 
        showParen (n >= 11) $ showString (varName ++ ": ") . showsPrec 0 a
      Just a -> showsPrec n a
    where
      varName = "_" ++ (varNames !! k)
      makeSupply inits tails = let vars = inits ++ (liftA2 (++) vars tails) in vars
      varNames = makeSupply (words "a b c d e f g h i j k") (words "1 2 3 4 5")

data Keys = Keys IntMap.Key Keys

data LVarData = LVarData Keys (IntMap.IntMap (Dynamic, IntSet.IntSet))

newtype LogicVarT m a = LogicVarT (StateT LVarData m a)
  deriving (Monad, MonadTrans, Applicative,
            Alternative, Functor, MonadPlus, MonadIO, MonadLogic)

runLogicVarT :: (Monad m) => LogicVarT m a -> m a
runLogicVarT (LogicVarT m) = evalStateT m (LVarData (keys 0) IntMap.empty)
  where
    keys n = Keys n $ keys (n + 1)

newUnboundLVar :: (Monad m, Data a) => LogicVarT m (LVar a)
newUnboundLVar = LogicVarT $ StateT $ insert Nothing

newBoundLVar :: (Monad m, Data a) => a -> LogicVarT m (LVar a)
newBoundLVar a = LogicVarT $ StateT $ insert (Just a)
      
insert :: (Data a, Monad m) => Maybe a -> LVarData -> m (LVar a, LVarData)
insert a (LVarData (Keys key next_keys) t) = 
  return (LVar key (Nothing `asTypeOf` a)
    , LVarData next_keys $ IntMap.insert key (toDyn a, IntSet.singleton key) t) 

readLVar :: (Monad m, Data a) => LVar a -> LogicVarT m (Maybe a)
readLVar (LVar key a) = LogicVarT $ gets $ \(LVarData _ t) -> 
  case IntMap.lookup key t of
    Just (x, _) -> join $ fromDynamic x
    Nothing -> a

bindLVar :: (Monad m) => LVar a -> LVar b -> LogicVarT m ()
bindLVar x y = LogicVarT $ modify $ bindLVar' x y
  where
    bindLVar' (LVar a _) (LVar b _) (LVarData keys t) | a == b = (LVarData keys t)
    bindLVar' (LVar key_a _) (LVar key_b _) (LVarData keys t) = fromMaybe (LVarData keys t) $ do
      (_, set_a) <- IntMap.lookup key_a t
      -- This guard will bail us out if these keys are already bound together
      guard $ not $ IntSet.member key_b set_a
      (v, set_b) <- IntMap.lookup key_b t
      let set' = IntSet.union set_a set_b
      return $ LVarData keys $ IntMap.union (fromSet (const (v, set')) set') t

fromSet :: (IntMap.Key -> a) -> IntSet.IntSet -> IntMap.IntMap a
fromSet f set = IntMap.fromList $ [ (k, f k) | k <- IntSet.toList set]

eqLVar :: (Monad m) => LVar a -> LVar a -> LogicVarT m Bool
eqLVar (LVar a _) (LVar b _) | a == b = return True
eqLVar (LVar a _) (LVar b _) = LogicVarT $ gets $ \(LVarData _ t) -> 
 case IntMap.lookup a t of
  Just (_, set) -> IntSet.member b set
  Nothing -> False

unrollLVars :: (Monad m) => GenericM (LogicVarT m)
unrollLVars = unrollLVars' IntSet.empty
  where
    unrollLVars' :: (Monad m) => IntSet.IntSet -> GenericM (LogicVarT m)
    unrollLVars' traversed a = ext1M (gmapM (unrollLVars' traversed)) (unrollLVar traversed) a

    unrollLVar :: (Monad m, Data a) => IntSet.IntSet -> LVar a -> LogicVarT m (LVar a)
    unrollLVar traversed (LVar key _) | IntSet.member key traversed = return $ LVar key Nothing
    unrollLVar traversed (LVar key a) = do
      (nextA, newKey) <- LogicVarT $ gets $ \(LVarData _ t) -> 
        case IntMap.lookup key t of
          Just (a', set) -> (((join $ fromDynamic a') `asTypeOf` a), IntSet.findMin set)
          Nothing -> (a, key)
      nextA' <- gmapM (unrollLVars' (IntSet.insert newKey traversed)) nextA
      return $ LVar newKey nextA'

reachable :: IntMap.Key -> IntSet.IntSet -> GenericQ Bool
reachable key traversed = ext1Q (or . gmapQ (reachable key traversed)) reachable'
  where
    reachable' :: (Data a) => LVar a -> Bool
    reachable' (LVar k Nothing) = k == key
    reachable' (LVar k _) | IntSet.member k traversed = False
    reachable' (LVar k (Just a)) = or $ gmapQ (reachable key (IntSet.insert k traversed)) a

