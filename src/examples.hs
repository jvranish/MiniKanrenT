{-#Language KindSignatures
          , TypeFamilies
          , FlexibleContexts
            #-}
 

import Control.Monad

import Control.Monad.Relational


-- make a symbol type
data Symbol (m :: * -> *) = Bob
                          | Lisa
                          | Joe
                          | Charlie
                          | Rose
  deriving (Eq, Ord, Show)


---- and the supporting instances
instance Unifiable Symbol where
  unifyValue a b | a == b = return ()
  unifyValue _ _ = mzero

instance Possibilities Symbol where
  possibilities = fmap return [Bob, Lisa, Joe, Charlie, Rose]

instance Evalable Symbol where
  eval _ Bob = return Bob
  eval _ Lisa = return Lisa
  eval _ Joe = return Joe
  eval _ Charlie = return Charlie
  eval _ Rose = return Rose





-- define a list type
data List f m = Cons (m (f m)) (m (List f m))
              | Nil

instance (LExpr ~ m, Show (f LExpr)) => Show (List f m) where
  showsPrec (-1) Nil = showString "]"
  showsPrec _ Nil = showString "[]"
  showsPrec (-1) (Cons x xs) = showString ", " . showsPrec 0 x . showsPrec (-1) xs
  showsPrec _ (Cons x xs) = showString "[" . showsPrec 0 x . showsPrec (-1) xs


--return list of fresh alternatives
instance (Possibilities f) => Possibilities (List f) where
  possibilities = 
    [ return Nil
    , liftM2 Cons fresh fresh
    ]

instance (Evalable f) => Evalable (List f) where
  eval _ Nil = return Nil
  eval f (Cons x xs) = do
    x' <- f x
    xs' <- f xs
    return $ Cons x' xs'

instance (Unifiable f) => Unifiable (List f) where
  unifyValue Nil Nil = return ()
  unifyValue (Cons a1 a2) (Cons b1 b2) = do
    a1 === b1
    a2 === b2
  unifyValue _ _ = mzero

-- A handy function to reify lists (convert a normal list into our logic list)
reifyList :: Monad m => (t -> m (f m)) -> [t] -> m (List f m)
reifyList _ [] = return Nil
reifyList f (x:xs) = return $ Cons (f x) (reifyList f xs)

-- A handy function to reify a list of symbols
rSymList :: Monad m => [f m] -> m (List f m)
rSymList xs = reifyList return xs








-- OK now we get to the actual interesting stuff
append :: Monad m => m (List f m) -> m (List f m) -> m (List f m)
append a b = flip append' b =<< a
  where
    append' Nil b' = b'
    append' (Cons x xs) b' = return $ Cons x (append xs b')

-- This is an interesting little function that takes a relational function
--  of: a -> b  and turns it into a: b -> a 
--  it reverses the function
un :: (Possibilities f, Unifiable g, MonadRelational m)
   => (m (f m) -> m (g m)) -> m (g m) -> m (f m)
un f a = do
  x <- fresh
  a === f x
  x

-- us 'un' to make unappend
unappend :: (Possibilities f, MonadRelational m)
         => m (List f m) -> m (List f m) -> m (List f m)
unappend a b = un (append a) b


member :: (Unifiable f, MonadRelational m) => m (f m) -> m (List f m) -> m ()
member x xs = member' =<< xs
  where
    member' (Cons h t) = h === x >> member x t
    member' Nil = mzero



testUnappend :: [LExpr (List Symbol LExpr)]
testUnappend = runRelation $ unappend (rSymList[Charlie]) (rSymList [Charlie, Lisa, Bob])



