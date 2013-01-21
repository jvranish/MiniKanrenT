
{-#Language DeriveDataTypeable #-}

module Control.Monad.MiniKanren.Symbol where

import Data.Data
import Data.String

import Control.Monad.MiniKanren.Core

data Symbol = Symbol String
  deriving (Show, Eq, Ord, Data, Typeable)

instance Unifiable Symbol where
  unifyValue (Symbol a) (Symbol b) | a == b = successful
  unifyValue _ _ = unsuccessful

instance IsString Symbol where
  fromString s = Symbol s

instance (IsString a) => IsString (Thunk m a) where
  fromString s = return $ new $ fromString s

reifySymbol :: (MonadKanren m) => String -> m (Thunk m Symbol)
reifySymbol s = return $ new $ Symbol s

