module Chapter3
  ( filter'
  , foldr'
  , foldl'
  , product'
  , all'
  ) where

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x
                   then x : (filter f xs )
                   else (filter f xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f init [] = init
foldr' f init (x:xs) = f x (foldr' f init xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f init [] = init
foldl' f init (x:xs) = f (foldl' f init xs) x

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * (product' xs)

all' :: [Bool] -> Bool
all' [] = True
all' (x:xs) = x && all' xs
