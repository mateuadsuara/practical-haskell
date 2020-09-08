module Chapter3
  ( filter'
  , foldr'
  , foldl'
  , product'
  , all'
  , minimumBy
  , elem'
  ) where

import Data.Maybe (fromMaybe)
import Data.List (find)

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

elem' :: (Eq a) => a -> [a] -> Bool
elem' e l = find (== e) l /= Nothing

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x l -> f x : l) []

-- PROOF
-- map (is aliased map') == mapAsFold (given foldr is aliased foldr')
-------------------------
--
--    mapAsFold f             = foldr (\x l -> f x : l) []
-- == mapAsFold f (x:xs)      = foldr (\x l -> f x : l) [] (x:xs)
--
-- BASE CASE
-------------------------
--
--    map f []                = []
-- => []
--
--    mapAsFold f []          = foldr (\x l -> f x : l) [] []
-- => foldr f=>_ init=>[] []  = init=>[]
-- => []
--
-- LONGER LIST (x:xs)
-------------------------
--
--    map f (x:xs)            = f x : map f xs
-- => f x : RECURSE_WITH xs
--
--    mapAsFold f (x:xs)      = foldr (\x l -> f x : l) [] (x:xs)
-- => foldr f'=>(\x l -> f x : l) init=>[] (x:xs)
--      = f'=>(\x l -> f x : l) x (foldr f init=>[] xs)
-- => (\x l -> f x : l) x (foldr f [] xs)
--      = f x : (foldr f [] xs)
-- => f x : RECURSE_WITH xs
