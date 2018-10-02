module Geometry.Cuboid
( volume
, area
) where

volume :: (Num a) => a -> a -> a -> a
volume l w h = l * w * h

area :: (Num a) => a -> a -> a -> a
area l w h = 2 * (w*l + h*l + h*w)
