module Chapter2.SimpleFunctions (firstOrEmpty, (+++), reverse2) where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

list1 +++ list2 =
  if null list1
  then list2
  else (head list1) : (tail list1) +++ list2

reverse2 :: [[Char]] -> [[Char]]
reverse2 list =
  if null list || (null $ tail list)
  then list
  else
    if null $ tail $ tail list
    then (head $ tail list) : head list : []
    else ((head (reverse2 (tail list))) : (tail (reverse2 (tail list))))
