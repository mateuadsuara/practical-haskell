module Chapter2.SimpleFunctions
  ( firstOrEmpty
  , (+++)
  , reverse2
  , maxmin
  , null'
  , head'
  , tail'
  , ackermann
  , unzip'
  , filterNumber
  , filterOnes
  , filterNot
  , filter'
  ) where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

list1 +++ list2 =
  if null list1
  then list2
  else (head list1) : (tail list1) +++ list2

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) =
  reverse2 xs +++ [x]

maxmin (max, min) list =
  if null list
  then (max, min)
  else let h = head list
       in maxmin ( if h > max then h else max
                 , if h < min then h else min
                 )
                 (tail list)

null' [] = True
null' _  = False

head' (x:xs) = x

tail' (x:xs) = xs

ackermann 0 n                  = n + 1
ackermann m 0 | m > 0          = ackermann (m - 1) 1
ackermann m n | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a, b):xs) = (a : as, b : bs)
  where (as, bs) = unzip' xs

filterNumber = filter . (==)

filterOnes = filterNumber 1

filterNot = filter . (not .)

filter' f [] = []
filter' f (x:xs) = if f x
                   then x : (filter f xs )
                   else (filter f xs)
