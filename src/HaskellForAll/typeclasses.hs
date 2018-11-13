{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellForAll.TypeClasses where

class WrappedThing a where
  getThing :: a -> Int

newtype MyA = MyA Int
newtype MyB = MyB Int
newtype MyC = MyC Int

instance WrappedThing MyA where
  getThing (MyA a) = a

instance WrappedThing MyB where
  getThing (MyB a) = a


class SuperAdder a b where
  combine :: a -> b -> Int

instance (WrappedThing a, WrappedThing b) => SuperAdder a b where
  combine a b =
    let a' = getThing a
        b' = getThing b
    in a' + b'

addStuff :: (WrappedThing a, WrappedThing f) => a -> f -> Int
addStuff a b =
  (combine a b) + 40


class SuperSub a b where
  subIt :: a -> b -> Int

newtype Sub a = Sub a

instance (WrappedThing a, WrappedThing b) => SuperSub (Sub a) (Sub b) where
  subIt (Sub a) (Sub b) =
    let a' = getThing a
        r = a' - combine a b
    in r

instance SuperSub Int Int where
  subIt a b = (a*b) - 20

instance SuperSub MyA MyC where
  subIt (MyA a) (MyC c) = (a*c) - (a `div` c)

doSubbs :: (SuperSub a b) => a -> b -> Int
doSubbs a b =
  subIt a b
