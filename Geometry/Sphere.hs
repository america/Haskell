module Geometry.Sphere
( volume
, area
) where

Volume :: Float -> Float
Volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

Area :: Float -> Float
Area radius = 4 * pi * (radius ^ 2)
       
