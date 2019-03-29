module Pyth where

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x 
    | x <= 0    = []
    | otherwise = do
        a <- [1..x]
        b <- [1..x]
        c <- [1..x]
        if a^2 + b^2 == c^2 then "z" else []
        return (a, b, c)
