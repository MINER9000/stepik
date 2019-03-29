module Expand where

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = 
                          let p1 = expand e1
                              p2 = expand e2
                              p  = expand e
                          in p1 :*: p :+: p2 :*: p
expand (e :*: (e1 :+: e2)) =
                          let p1 = expand e1
                              p2 = expand e2
                              p  = expand e
                          in p :*: p1 :+: p :*: p2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e
