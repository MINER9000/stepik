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
