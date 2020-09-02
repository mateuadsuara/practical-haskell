module Chapter3
  ( filter'
  , foldr'
  ) where

filter' f [] = []
filter' f (x:xs) = if f x
                   then x : (filter f xs )
                   else (filter f xs)

foldr' f init [] = init
foldr' f init (x:xs) = f x (foldr' f init xs)
