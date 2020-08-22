module Main (main) where

import System.Exit

import Chapter2
import Chapter2.SimpleFunctions

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
  test ( "firstOrEmpty first"
       , (firstOrEmpty ["abc"]) == "abc"
       )
  test ( "firstOrEmpty empty"
       , (firstOrEmpty []) == "empty"
       )
  test ( "+++ empty"
       , ([] +++ ["abc"]) == ["abc"]
       )
  test ( "+++ two"
       , (["abc"] +++ ["de"]) == ["abc", "de"]
       )
  tsEq ( "reverse2 empty"
       , (reverse2 [])
       , []
       )
  tsEq ( "reverse2 one"
       , (reverse2 ["abc"])
       , ["abc"]
       )
  tsEq ( "reverse2 two"
       , (reverse2 ["abc", "de"])
       , ["de", "abc"]
       )
  tsEq ( "reverse2 three"
       , (reverse2 ["abc", "de", "fg"])
       , ["fg", "de", "abc"]
       )
  tsEq ( "reverse2 many"
       , (reverse2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"])
       , ["9", "8", "7", "6", "5", "4", "3", "2", "1"]
       )

test (d, b) =
  if b
  then putStrLn $ "PASS " ++ d
  else do
    putStrLn $ "FAIL " ++ d
    exitFailure

tsEq (d, r, e) =
  if r == e
  then putStrLn $ "PASS " ++ d
  else do
    putStrLn $ "FAIL " ++ d ++ ", got: " ++ (show r)
    exitFailure
