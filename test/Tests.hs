module Main (main) where

import System.Exit

import Chapter2
import Chapter2.SimpleFunctions
import Store.Client
import Store.TimeMachine
import Chapter3

main :: IO ()
main = do
  test "2.1.a"
       Chapter2.exercise_2_1_a
       (==)
       ["abc", "de"]

  test "2.1.b 1"
       (Chapter2.exercise_2_1_b [])
       (==)
       True

  test "2.1.b 2"
       (Chapter2.exercise_2_1_b [[], ['a', 'b']])
       (==)
       True

  test "2.1.b 3"
       (Chapter2.exercise_2_1_b [['a', 'b']])
       (==)
       False

  test "2.1.c 1"
       (Chapter2.exercise_2_1_c [])
       (==)
       False

  test "2.1.c 2"
       (Chapter2.exercise_2_1_c ['a'])
       (==)
       True

  test "2.1.c 3"
       (Chapter2.exercise_2_1_c ['a', 'b'])
       (==)
       False

  test "2.1.d"
       (Chapter2.exercise_2_1_d ["abc", "de"])
       (==)
       "abcde"

  test "firstOrEmpty first"
       (firstOrEmpty ["abc"])
       (==)
       "abc"

  test "firstOrEmpty empty"
       (firstOrEmpty [])
       (==)
       "empty"

  test "+++ empty"
       ([] +++ ["abc"])
       (==)
       ["abc"]

  test "+++ two"
       (["abc"] +++ ["de"])
       (==)
       ["abc", "de"]

  test "reverse2 empty"
       (reverse2 [] :: [String])
       (==)
       []

  test "reverse2 one"
       (reverse2 ["abc"])
       (==)
       ["abc"]

  test "reverse2 two"
       (reverse2 ["abc", "de"])
       (==)
       ["de", "abc"]

  test "reverse2 three"
       (reverse2 ["abc", "de", "fg"])
       (==)
       ["fg", "de", "abc"]

  test "reverse2 many"
       (reverse2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"])
       (==)
       ["9", "8", "7", "6", "5", "4", "3", "2", "1"]

  test "maxmin empty"
       (maxmin (0, 0) [])
       (==)
       (0, 0)

  test "maxmin one a"
       (maxmin (0, 2) [1])
       (==)
       (1, 1)

  test "maxmin one b"
       (maxmin (2, 0) [1])
       (==)
       (2, 0)

  test "maxmin two"
       (maxmin (-999, 999) [2, 1])
       (==)
       (2, 1)

  test "maxmin many"
       (maxmin (-999, 999) [2, 1, 103, 8, 75, -300])
       (==)
       (103, -300)

  test "show client"
       (show $ Individual (Person "Jack" "Smith" Male))
       (==)
       "Individual (Person \"Jack\" \"Smith\" Male)"

  test "clientName GovOrg"
       (clientName $ GovOrg "organisation")
       (==)
       "organisation"

  test "clientName Company"
       (clientName $ Company "Comp Limited" 1 (Person "Jane" "Doe" Female) "Resp")
       (==)
       "Comp Limited"

  test "clientName Individual"
       (clientName $ Individual (Person "Jack" "Smith" Male))
       (==)
       "Jack Smith"

  test "show time machine"
       (show $ TimeMachine { manufacturer = "Tesla"
                           , model = 3
                           , name = "The E in SEXY"
                           , travelDirection = Future
                           , price = 35000.00
                           })
       (==)
       "TimeMachine {manufacturer = \"Tesla\", model = 3, name = \"The E in SEXY\", travelDirection = Future, price = 35000.0}"

  test "countGenders empty"
       (countGenders [])
       (==)
       (0, 0, 0)

  test "countGenders one male"
       (countGenders [(Individual (Person "A" "B" Male))])
       (==)
       (1, 0, 0)

  test "countGenders one female"
       (countGenders [(Individual (Person "A" "B" Female))])
       (==)
       (0, 1, 0)

  test "countGenders one unknown"
       (countGenders [(Individual (Person "A" "B" Unknown))])
       (==)
       (0, 0, 1)

  test "countGenders GovOrg"
       (countGenders [(GovOrg "A")])
       (==)
       (0, 0, 0)

  test "countGenders Company"
       (countGenders [(Company "A" 1 (Person "A" "B" Male) "B")])
       (==)
       (0, 0, 0)

  test "countGenders one of each"
       (countGenders [ (Individual (Person "A" "B" Male))
                     , (Individual (Person "A" "B" Female))
                     , (Individual (Person "A" "B" Unknown))
                     ])
       (==)
       (1, 1, 1)

  test "applyPriceMultiplier 50% to two TimeMachines"
       (applyPriceMultiplier 0.5 [ TimeMachine { manufacturer = "Tesla"
                                               , model = 3
                                               , name = "The E in SEXY"
                                               , travelDirection = Future
                                               , price = 35000.00
                                               }
                                 , TimeMachine { manufacturer = "Tesla"
                                               , model = 1
                                               , name = "The mass market"
                                               , travelDirection = Future
                                               , price = 15000.00
                                               }
                                 ])
       (==)
       [ TimeMachine { manufacturer = "Tesla"
                     , model = 3
                     , name = "The E in SEXY"
                     , travelDirection = Future
                     , price = 17500.00
                     }
       , TimeMachine { manufacturer = "Tesla"
                     , model = 1
                     , name = "The mass market"
                     , travelDirection = Future
                     , price = 7500.00
                     }
       ]

  test "null' empty"
       (null' [])
       (==)
       True

  test "null' one"
       (null' [1])
       (==)
       False

  test "head' one"
       (head' [1])
       (==)
       1

  test "tail' three"
       (tail' [1, 2, 3])
       (==)
       [2, 3]

  test "ackermann 0 0"
       (ackermann 0 0)
       (==)
       1

  test "ackermann 0 1"
       (ackermann 0 1)
       (==)
       2

  test "ackermann 1 0"
       (ackermann 1 0)
       (==)
       2

  test "ackermann 1 1"
       (ackermann 1 1)
       (==)
       3

  test "ackermann 0 2"
       (ackermann 0 2)
       (==)
       3

  test "ackermann 2 0"
       (ackermann 2 0)
       (==)
       3

  test "ackermann 1 2"
       (ackermann 1 2)
       (==)
       4

  test "ackermann 2 1"
       (ackermann 2 1)
       (==)
       5

  test "ackermann 2 2"
       (ackermann 2 2)
       (==)
       7

  test "unzip' empty"
       (unzip' [] :: ([Int], [String]))
       (==)
       ([], [])

  test "unzip' one"
       (unzip' [(1, "a")])
       (==)
       ([1], ["a"])

  test "unzip' two"
       (unzip' [(1, "a"), (2, "b")])
       (==)
       ([1, 2], ["a", "b"])

  test "filterOnes"
       (filterOnes [1, 4, 2, 1, 2, 3, 6])
       (==)
       [1, 1]

  test "filterNumber"
       (filterNumber 5 [1, 2, 3, 4, 5, 6, 7, 8, 1, 3, 5, 7])
       (==)
       [5, 5]

  test "filterNot"
       (filterNot (\x -> x == 1) [1, 2, 3])
       (==)
       [2, 3]

  test "filterGovOrgs"
       (filterGovOrgs [ (Company "A" 1 (Person "A" "B" Male) "B")
                      , (GovOrg "A")
                      , (Individual (Person "A" "B" Unknown))
                      ])
       (==)
       [ (GovOrg "A") ]

  test "filter'"
       (filter' (\x -> 0 == (mod x 2)) [1, 2, 3, 4, 5, 6])
       (==)
       [2, 4, 6]

  test "foldr'"
       (foldr' (+) 0 [1, 2, 3, 4])
       (==)
       10

  test "foldl'"
       (foldr' (+) 0 [1, 2, 3, 4])
       (==)
       10

  test "product' empty"
       (product' [])
       (==)
       1

  test "product' one"
       (product' [7])
       (==)
       7

  test "product' many"
       (product' [1, 2, 3, 4, 5])
       (==)
       120

  test "minimumClient one"
       (minimumClient [ (Individual (Person "A" "B" Unknown))
                      ])
       (==)
       (Individual (Person "A" "B" Unknown))

  test "minimumClient three"
       (minimumClient [ (Company "ABCDE" 1 (Person "A" "B" Male) "B")
                      , (GovOrg "ABC")
                      , (Individual (Person "A" "BC" Unknown))
                      ])
       (==)
       (GovOrg "ABC")

test :: Show a => String -> a -> (a -> b -> Bool) -> b -> IO ()
test d r f e =
  if f r e
  then putStrLn $ "PASS " ++ d
  else do
    putStrLn $ "FAIL " ++ d ++ ", result is: " ++ (show r)
    exitFailure
