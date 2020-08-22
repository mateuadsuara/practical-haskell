module MyLib (someFunc, exercise_2_1_a, exercise_2_1_b, exercise_2_1_c, exercise_2_1_d) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

exercise_2_1_a = ('a' : 'b' : 'c' : []) : ('d' : 'e' : []) : []

exercise_2_1_b :: [[a]] -> Bool
exercise_2_1_b l = (null l) || (null (head l))

exercise_2_1_c :: [a] -> Bool
exercise_2_1_c l = (not (null l)) && (null (tail l))

exercise_2_1_d :: [[Char]] -> [Char]
exercise_2_1_d l = (head l) ++ (head (tail l))
