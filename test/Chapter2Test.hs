module Main (main) where

import Chapter2

main :: IO ()
main = do
  test ( "2.1.a"
       , Chapter2.exercise_2_1_a == ["abc", "de"]
       )
  test ( "2.1.b 1"
       , (Chapter2.exercise_2_1_b []) == True
       )
  test ( "2.1.b 2"
       , (Chapter2.exercise_2_1_b [[], ['a', 'b']]) == True
       )
  test ( "2.1.b 3"
       , (Chapter2.exercise_2_1_b [['a', 'b']]) == False
       )
  test ( "2.1.c 1"
       , (Chapter2.exercise_2_1_c []) == False
       )
  test ( "2.1.c 2"
       , (Chapter2.exercise_2_1_c ['a']) == True
       )
  test ( "2.1.c 3"
       , (Chapter2.exercise_2_1_c ['a', 'b']) == False
       )
  test ( "2.1.d"
       , (Chapter2.exercise_2_1_d ["abc", "de"]) == "abcde"
       )

test (d, b) =
  if b then
    putStrLn $ "PASS " ++ d
  else
    putStrLn $ "FAIL " ++ d
