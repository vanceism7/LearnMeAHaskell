import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid

sequenceB1 :: (Applicative f) => [f a] -> f [a]
sequenceB1 [] = pure []
sequenceB1 (x:xs) =
   (:) <$> x <*> sequenceB1 xs

sequenceB2 xs =
  foldr (\x acc -> (:) <$> x <*> acc) (pure []) xs

add3 :: (Num a) => a -> a -> a -> a
add3 a b c = a + b + c


-----------------
--newtype keyword
--------

newtype Pair b a
  = Pair { getPair :: (a,b) }
  deriving (Show, Eq, Read)

instance Functor (Pair c) where
  fmap f (Pair (a,b)) = Pair (f a, b)




--------------
--Monoids--
-------

newtype CharList = CharList {getCharList :: [Char]} deriving (Show, Eq, Ord)

instance Semigroup CharList where
  CharList l1 <> CharList l2 = CharList $ l1 ++ l2

instance Monoid CharList where
  mempty = CharList []


data SpecOrd = Smaller | Equal | Bigger deriving (Show, Read)

-- -- First attempt haha
-- instance Semigroup SpecOrd where
--   Smaller <> Smaller = Equal
--   Smaller <> _ = Smaller
--   Bigger <> Bigger = Equal
--   Bigger <> _ = Bigger
--   Equal <> Equal = Equal
--   Equal <> Bigger = Smaller
--   Equal <> Smaller = Bigger

instance Semigroup SpecOrd where
  Smaller <> _ = Smaller
  Equal <> x = x
  Bigger <> _ = Bigger

instance Monoid SpecOrd where
  mempty = Equal


----------------
-- Foldable
------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node x l r ) = Node (f x) (fmap f l) (fmap f r)

instance F.Foldable Tree where
  --foldMap f (Node x l r) = f x <> foldMap f l <> foldMap f r
  foldMap f Empty = mempty
  foldMap f (Node x l r) = foldMap f l <> f x <> foldMap f r
