module Geometry.Cuboid
( volume
, area
) where

Volume :: Float -> Float -> Float -> Float
Volume a b c = rectArea a b * c

Area :: Float -> Float -> Float -> Float
Area a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2       

rectArea :: Float -> Float -> Float
rectArea a b = a * b
