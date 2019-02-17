module Hello where

import Data.Char


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then read $ x : y : [] else 100

lenVec3 :: Float -> Float -> Float -> Float
lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign :: Integer -> Integer
sign x = if x == 1 then 0 else (if x > 0 then 1 else (-1))

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, x2) (y1, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

fibonacci :: Integer -> Integer
fibonacci n = helper 0 1 n
  where helper f1 f2 n | n == 0 = f1
                       | n == 1 = f2
                       | n > 0  = helper f2 (f1 + f2) (n - 1)
                       | n < 0  = helper (f2 - f1) f1 (n + 1)

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = let d = discriminant a b c in (root a b c d, root a b c d) 
  where root a b c d = (-b + d) / (2 * a)
        discriminant a b c = sqrt (b * b - 4 * a * c)

seqA :: Integer -> Integer
seqA n = helper 1 2 3 n
  where helper a1 a2 a3 n | n == 0 = a1
                          | n == 1 = a2
                          | n == 2 = a3
                          | n > 2  = helper a2 a3 (a2 + a3 - 2 * a1) (n - 1)
