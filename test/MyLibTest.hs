module Main (main) where

import MyLib

main :: IO ()
main = do
  test ( "2.1.a"
       , MyLib.exercise_2_1_a == ["abc", "de"]
       )
  test ( "2.1.b 1"
       , (MyLib.exercise_2_1_b []) == True
       )
  test ( "2.1.b 2"
       , (MyLib.exercise_2_1_b [[], ['a', 'b']]) == True
       )
  test ( "2.1.b 3"
       , (MyLib.exercise_2_1_b [['a', 'b']]) == False
       )
  test ( "2.1.c 1"
       , (MyLib.exercise_2_1_c []) == False
       )
  test ( "2.1.c 2"
       , (MyLib.exercise_2_1_c ['a']) == True
       )
  test ( "2.1.c 3"
       , (MyLib.exercise_2_1_c ['a', 'b']) == False
       )
  test ( "2.1.d"
       , (MyLib.exercise_2_1_d ["abc", "de"]) == "abcde"
       )

test (d, b) =
  if b then
    putStrLn $ "PASS " ++ d
  else
    putStrLn $ "FAIL " ++ d
