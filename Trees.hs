module Trees where

data Tree a = Nil
            | Branch (Tree a) a (Tree a)
    deriving Show

newtype Preorder a = PreO (Tree a) deriving Show
newtype Postorder a = PostO (Tree a) deriving Show
newtype Levelorder a = LevelO (Tree a) deriving Show

tree :: Tree Int
tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

instance Foldable Tree where
  --foldr :: (a -> b -> b) -> b -> t a -> b
    foldr _ ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Foldable Preorder where
    foldr _ ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r))
         = f x (foldr f (foldr f ini (PreO r)) (PreO l))

--x :: a
--ini :: b
--foldr f _ l :: b
--foldr f _ r :: b
--f :: a -> b -> b

instance Foldable Postorder where
    foldr _ ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r))
         = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
    foldr f ini tree = foldr f ini (toListTree [tree])

toListTree :: [Levelorder a] -> [a]
toListTree [] = []
toListTree (t:ts)
    = case t of
      (LevelO (Branch l x r)) -> x:toListTree (ts++[LevelO l]++[LevelO r])
      (LevelO Nil)            -> toListTree ts
