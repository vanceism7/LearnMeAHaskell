module Ch8 where

import qualified Data.Map as Map
import qualified Data.Either as Either

data Mybool = Right | Wrong

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)

translate :: Shape -> Float -> Float -> Shape

-- Moves a circle
translate (Circle (Point x y) r) mvx mvy  =
  (Circle (Point (x+mvx) (y+mvy)) r)

-- Moves a Rectangle
translate (Rectangle (Point x1 y1) (Point x2 y2)) mvx mvy =
  (Rectangle (Point (x1+mvx) (y1+mvy)) (Point (x2+mvx) (y2+mvy)))

-- Some Person type and function
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- Vector type
data Vector t = Vector t t t deriving (Show, Read, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector x y z) = Vector (i+x) (j+y) (k+z)

scalarMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `scalarMult` s = Vector (i*s) (j*s) (k*s)

dotProduct :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `dotProduct` (Vector x y z) = (i*x) + (j*y) + (k*z)

magnitude :: (Floating t) => Vector t -> t
magnitude v = sqrt $ v `dotProduct` v

------------------
-- Day of the Week
-----
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


----------
--Lockers
-----

(|>) :: a -> (a -> b) -> b
a |> f = f a

data Locker = Locker { code :: String, taken :: Bool }
-- type Locker = (String,Bool)
type LockerList = Map.Map Int Locker

requestLocker :: Int -> LockerList -> Either String String
requestLocker lockerNum lockers =
    case Map.lookup lockerNum lockers of
      Nothing -> Either.Left "Locker not found"
      Just locker ->
        if locker |> taken then
          Either.Left "Locker in use"
        else
          Either.Right $ locker |> code


-----------------------
-- Binary Search Tree
----------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

tInsert :: (Ord a) => a -> Tree a -> Tree a

tInsert a Empty = Node a Empty Empty

tInsert a (Node v left right) =
  case a `compare` v of
    GT -> Node v left (tInsert a right)
    otherwise -> Node v (tInsert a left) right


tElem :: (Ord a) => a -> Tree a -> Bool

tElem _ Empty = False

tElem a (Node v left right)
  | a == v = True
  | a < v = tElem a left
  | a > v = tElem a right


------------------
--Typeclasses 102
-----------

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "STOP!"
  show Yellow = "Yellow Light"
  show Green = "Green Light!"


----------
--YesNo
----

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int  where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo (Tree a) where
  yesno Empty = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

instance (YesNo m) => YesNo (Maybe m) where
  yesno (Just a) = yesno a
  yesno Nothing = False


-----------
--Functor
-----

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

data Dictionary k v = Dictionary [(k,v)] deriving (Show, Eq)

instance Functor (Dictionary k) where
  fmap f (Dictionary d) =
    let
      apply [] = []
      apply ((key,value):ds) = (key, f value):apply ds
    in Dictionary $ apply d


------------------------
-- Kinds and Type-Foo
------------

class Tofu t where
  tofu :: j a -> t a j

data Jen = Jennifer

data Bob a = Bob { bobby :: a }

data Frank a b = Frank { frankie :: b a } deriving (Show)

instance Tofu Frank where
  tofu x = Frank x


data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)

instance Functor (Barry a b) where
  -- fmap f x = f x 
  fmap f (Barry { yabba = x, dabba = y }) = (Barry { yabba = f x, dabba = y })
