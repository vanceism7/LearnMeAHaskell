module Geometry.Sphere
( volume
, area
) where

volume :: (RealFloat a) => a -> a
volume r = (4/3) * pi * (r^3)

area :: (RealFloat a) => a -> a
area r = 4 * pi * r^2
