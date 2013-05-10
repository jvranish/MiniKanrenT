{-#Language GADTs
          , GeneralizedNewtypeDeriving
          , RankNTypes
          #-}

module Control.Monad.PureRefT 
  ( runPureRefT
  , PureRefT
  , Ref
  , newRef
  , insert
  , readRef
  , writeRef
  , bulkWriteRef
  , refKey
  ) where 

import Control.Monad.Trans.State 
import Control.Monad.Trans.Class
import Control.Monad.Reader  hiding (msum, mapM_)
import Control.Monad.Logic hiding (msum, mapM_)
import Control.Applicative

import Data.Foldable
import Data.Maybe
import qualified Data.IntMap as IntMap

import Unsafe.Coerce

import Prelude hiding (mapM_)

data Keys = Keys IntMap.Key Keys
data Hidden where
  Hidden :: a -> Hidden
data RefData = RefData Keys (IntMap.IntMap Hidden)

newtype PureRefT s m a = PureRefT (StateT RefData m a)
  deriving (Monad, MonadTrans, Applicative,
            Alternative, Functor, MonadPlus, MonadIO, MonadLogic)

newtype Ref s a = Ref IntMap.Key

runPureRefT :: (Monad m) => (forall s. PureRefT s m a) -> m a
runPureRefT m' = run m'
  where
    keys n = Keys n $ keys (n + 1)
    run :: (Monad m) => PureRefT () m a -> m a
    run (PureRefT m) = evalStateT m (RefData (keys 0) IntMap.empty)

newRef :: (Monad m) => a -> PureRefT s m (Ref s a)
newRef a = PureRefT $ StateT $ insert a

insert :: (Monad m) => a -> RefData -> m (Ref s a, RefData)
insert a (RefData (Keys key next_keys) t) = 
  return (Ref key
    , RefData next_keys $ IntMap.insert key (Hidden a) t) 

readRef :: (Monad m) => Ref s a -> PureRefT s m a
readRef (Ref key) = PureRefT $ gets $ \(RefData _ t) -> 
  case IntMap.lookup key t of
    -- I could use Dynamic here but it requires Typeable, which is kinda a 
    -- pain for ghc <= 7.6
    Just (Hidden x) -> unsafeCoerce x 
    Nothing -> error "badness"  -- this should never happen

writeRef :: (Monad m) => Ref s a -> a -> PureRefT s m ()
writeRef (Ref key) = PureRefT . StateT . doInsert key
  where
    doInsert k a (RefData keys t) =
      return ((), RefData keys $ IntMap.insert k (Hidden a) t) 

-- #TODO replace this with something more efficient
bulkWriteRef :: (Monad m) => [Ref s a] -> a -> PureRefT s m ()
bulkWriteRef refs a = mapM_ (flip writeRef a) refs

refKey :: Ref s a -> Int
refKey (Ref k) = k

