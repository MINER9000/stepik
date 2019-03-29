module Coord where

data Coord a = Coord a a
    deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter breadth (Coord x y) = Coord (toCenter x) (toCenter y)
  where 
    toCenter c = (fromIntegral c) * breadth + (breadth / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell breadth (Coord x y) = Coord (toCell x) (toCell y)
  where toCell c = floor (c / breadth)
