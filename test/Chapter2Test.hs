module Main (main) where

import System.Exit

import Chapter2
import Chapter2.SimpleFunctions
import Store.TimeMachine

main :: IO ()
main = do
  test ( "2.1.a"
       , Chapter2.exercise_2_1_a
       , (==)
       , ["abc", "de"]
       )
  test ( "2.1.b 1"
       , Chapter2.exercise_2_1_b []
       , (==)
       , True
       )
  test ( "2.1.b 2"
       , Chapter2.exercise_2_1_b [[], ['a', 'b']]
       , (==)
       , True
       )
  test ( "2.1.b 3"
       , Chapter2.exercise_2_1_b [['a', 'b']]
       , (==)
       , False
       )
  test ( "2.1.c 1"
       , Chapter2.exercise_2_1_c []
       , (==)
       , False
       )
  test ( "2.1.c 2"
       , Chapter2.exercise_2_1_c ['a']
       , (==)
       , True
       )
  test ( "2.1.c 3"
       , Chapter2.exercise_2_1_c ['a', 'b']
       , (==)
       , False
       )
  test ( "2.1.d"
       , Chapter2.exercise_2_1_d ["abc", "de"]
       , (==)
       , "abcde"
       )
  test ( "firstOrEmpty first"
       , firstOrEmpty ["abc"]
       , (==)
       , "abc"
       )
  test ( "firstOrEmpty empty"
       , firstOrEmpty []
       , (==)
       , "empty"
       )
  test ( "+++ empty"
       , [] +++ ["abc"]
       , (==)
       , ["abc"]
       )
  test ( "+++ two"
       , ["abc"] +++ ["de"]
       , (==)
       , ["abc", "de"]
       )
  test ( "reverse2 empty"
       , reverse2 []
       , (==)
       , []
       )
  test ( "reverse2 one"
       , reverse2 ["abc"]
       , (==)
       , ["abc"]
       )
  test ( "reverse2 two"
       , reverse2 ["abc", "de"]
       , (==)
       , ["de", "abc"]
       )
  test ( "reverse2 three"
       , reverse2 ["abc", "de", "fg"]
       , (==)
       , ["fg", "de", "abc"]
       )
  test ( "reverse2 many"
       , reverse2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
       , (==)
       , ["9", "8", "7", "6", "5", "4", "3", "2", "1"]
       )
  test ( "maxmin empty"
       , maxmin (0, 0) []
       , (==)
       , (0, 0)
       )
  test ( "maxmin one a"
       , maxmin (0, 2) [1]
       , (==)
       , (1, 1)
       )
  test ( "maxmin one b"
       , maxmin (2, 0) [1]
       , (==)
       , (2, 0)
       )
  test ( "maxmin two"
       , maxmin (-999, 999) [2, 1]
       , (==)
       , (2, 1)
       )
  test ( "maxmin many"
       , maxmin (-999, 999) [2, 1, 103, 8, 75, -300]
       , (==)
       , (103, -300)
       )
  test ( "show time machine"
       , show $ TimeMachine (Manufacturer "Tesla") (Model 3) (Name "The E in SEXY") (Future) (Price 35000.00)
       , (==)
       , "TimeMachine (Manufacturer \"Tesla\") (Model 3) (Name \"The E in SEXY\") Future (Price 35000.0)"
       )

test (d, r, f, e) =
  if f r e
  then putStrLn $ "PASS " ++ d
  else do
    putStrLn $ "FAIL " ++ d ++ ", result is: " ++ (show r)
    exitFailure
