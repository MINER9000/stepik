module FactorialTest where

factorial :: Integer -> Integer
factorial n | n >= 0    = helper 1 n
            | otherwise = error "arg must be >= 0"
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)

factorial' :: Integer -> Integer
factorial' n | n >= 0    = helper 1 n
             | otherwise = error "arg must be >= 0"
  where
    helper acc 0 = acc
    helper acc n = (helper $! (acc * n)) (n - 1)
