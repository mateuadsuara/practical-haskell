module Chapter2.SimpleFunctions (firstOrEmpty, (+++), reverse2, maxmin, null', head', tail', ackermann, unzip') where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

list1 +++ list2 =
  if null list1
  then list2
  else (head list1) : (tail list1) +++ list2

reverse2 :: [a] -> [a]
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
