{-#Language GeneralizedNewtypeDeriving
          , DeriveDataTypeable #-}
module Control.Monad.LogicVarT 
  ( LogicVarT
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

import Data.Generics.Aliases

data LVar a = LVar { lvarKey :: IntMap.Key, lvarValue :: Maybe a }
  deriving (Eq, Ord, Data, Typeable)

instance (Data a, Show a) => Show (LVar a) where
  show (LVar k Nothing) = "_" ++ (varNames !! k)
  show (LVar k (Just a)) | reachable k IntSet.empty a = "( _" ++ (varNames !! k) ++ ": " ++ show a ++ ")"
  show (LVar _ (Just a)) = show a


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
      (v, set_b) <- IntMap.lookup key_b t -- #TODO if this fails, need to insert into map?  yes
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
    reachable' (LVar k (Just a)) = or $ gmapQ (reachable key (IntSet.insert k traversed)) a

makeSupply :: [[a]] -> [[a]] -> [[a]]
makeSupply inits tails = let vars = inits ++ (liftA2 (++) vars tails) in vars

varNames :: [String]
varNames = makeSupply (words "A B C D E F G H I J K") (words "1 2 3 4 5")

