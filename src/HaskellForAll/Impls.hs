module HaskellForAll.Impls where

import Data.Traversable

-------------------
------ MapM
---

class (Functor t, Foldable t) => MMapper t where
  mySeq :: Monad m => t (m a) -> m (t a)
  myMapM :: Monad m => (a -> m b) -> t a -> m (t b)

specialInt :: Int -> Maybe Int
specialInt x =
  if x < 20 then
    Just $ x+10
  else
    Nothing

instance MMapper [] where
  mySeq [] = return []
  mySeq (x:xs) =
     (:) <$> x <*> mySeq xs

  myMapM f x =
    mySeq $ fmap f x
