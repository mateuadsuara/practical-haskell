module Chapter2.SimpleFunctions (firstOrEmpty, (+++), reverse2, maxmin) where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

list1 +++ list2 =
  if null list1
  then list2
  else (head list1) : (tail list1) +++ list2

reverse2 :: [[Char]] -> [[Char]]
reverse2 list =
  if null list
  then []
  else reverse2 (tail list) +++ [head list]

maxmin (max, min) list =
  if null list
  then (max, min)
  else let h = head list
       in maxmin ( if h > max then h else max
                 , if h < min then h else min
                 )
                 (tail list)
