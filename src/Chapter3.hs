module Chapter3
  ( filter'
  , foldr'
  , foldl'
  , product'
  , all'
  , minimumBy
  ) where

import Data.Maybe (fromMaybe)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x
                   then x : (filter f xs)
                   else (filter f xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f init [] = init
foldr' f init (x:xs) = f x (foldr' f init xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f init [] = init
foldl' f init (x:xs) = f (foldl' f init xs) x

product' :: [Int] -> Int
product' = foldl' (*) 1

all' :: [Bool] -> Bool
all' = foldl' (&&) True

minimumBy :: (Ord b) => (a -> b) -> [a] -> Maybe a
minimumBy fn lst = foldr' aggFn Nothing lst
  where aggFn x maybeMinimum =
          let minimum = fromMaybe x maybeMinimum in
          if (fn x) < (fn minimum)
          then Just x
          else Just minimum
