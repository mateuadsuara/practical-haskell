module Chapter2.SimpleFunctions (firstOrEmpty, (+++)) where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

list1 +++ list2 =
  if null list1
  then list2
  else (head list1) : (tail list1) +++ list2
