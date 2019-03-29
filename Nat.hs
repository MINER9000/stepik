module Nat where

data Nat = Zero | Suc Nat
    deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

intToNat :: Integer -> Nat
intToNat 0 = Zero
intToNat n = (n - 1) `seq` Suc $ intToNat (n - 1)

add :: Nat -> Nat -> Nat
add x y = intToNat (fromNat x + fromNat y)

mul :: Nat -> Nat -> Nat
mul x y = intToNat (fromNat x * fromNat y)

fac :: Nat -> Nat
fac n = intToNat $ helper 1 (fromNat n)
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)
