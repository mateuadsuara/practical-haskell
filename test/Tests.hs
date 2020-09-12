module Main (main) where

import Test.Hspec

import Chapter2
import Chapter2.SimpleFunctions
import Store.Client
import Store.TimeMachine
import Chapter3

main :: IO ()
main = hspec $ do
  it "2.1.a" $ do
       Chapter2.exercise_2_1_a
       `shouldBe`
       ["abc", "de"]

  it "2.1.b" $ do
       Chapter2.exercise_2_1_b []
       `shouldBe`
       True

  it "2.1.b 2" $ do
       Chapter2.exercise_2_1_b [[], ['a', 'b']]
       `shouldBe`
       True

  it "2.1.b 3" $ do
       Chapter2.exercise_2_1_b [['a', 'b']]
       `shouldBe`
       False

  it "2.1.c 1" $ do
       Chapter2.exercise_2_1_c []
       `shouldBe`
       False

  it "2.1.c 2" $ do
       Chapter2.exercise_2_1_c ['a']
       `shouldBe`
       True

  it "2.1.c 3" $ do
       Chapter2.exercise_2_1_c ['a', 'b']
       `shouldBe`
       False

  it "2.1.d" $ do
       Chapter2.exercise_2_1_d ["abc", "de"]
       `shouldBe`
       "abcde"

  it "firstOrEmpty first" $ do
       firstOrEmpty ["abc"]
       `shouldBe`
       "abc"

  it "firstOrEmpty empty" $ do
       firstOrEmpty []
       `shouldBe`
       "empty"

  it "+++ empty" $ do
       [] +++ ["abc"]
       `shouldBe`
       ["abc"]

  it "+++ two" $ do
       ["abc"] +++ ["de"]
       `shouldBe`
       ["abc", "de"]

  it "reverse2 empty" $ do
       reverse2 [] :: [String]
       `shouldBe`
       []

  it "reverse2 one" $ do
       reverse2 ["abc"]
       `shouldBe`
       ["abc"]

  it "reverse2 two" $ do
       reverse2 ["abc", "de"]
       `shouldBe`
       ["de", "abc"]

  it "reverse2 three" $ do
       reverse2 ["abc", "de", "fg"]
       `shouldBe`
       ["fg", "de", "abc"]

  it "reverse2 many" $ do
       reverse2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
       `shouldBe`
       ["9", "8", "7", "6", "5", "4", "3", "2", "1"]

  it "maxmin empty" $ do
       maxmin (0, 0) []
       `shouldBe`
       (0, 0)

  it "maxmin one a" $ do
       maxmin (0, 2) [1]
       `shouldBe`
       (1, 1)

  it "maxmin one b" $ do
       maxmin (2, 0) [1]
       `shouldBe`
       (2, 0)

  it "maxmin two" $ do
       maxmin (-999, 999) [2, 1]
       `shouldBe`
       (2, 1)

  it "maxmin many" $ do
       maxmin (-999, 999) [2, 1, 103, 8, 75, -300]
       `shouldBe`
       (103, -300)

  it "show client" $ do
       show $ Individual (Person "Jack" "Smith" Male)
       `shouldBe`
       "Individual (Person \"Jack\" \"Smith\" Male)"

  it "clientName GovOrg" $ do
       clientName $ GovOrg "organisation"
       `shouldBe`
       "organisation"

  it "clientName Company" $ do
       clientName $ Company "Comp Limited" 1 (Person "Jane" "Doe" Female) "Resp"
       `shouldBe`
       "Comp Limited"

  it "clientName Individual" $ do
       clientName $ Individual (Person "Jack" "Smith" Male)
       `shouldBe`
       "Jack Smith"

  it "show time machine" $ do
       show $ TimeMachine { manufacturer = "Tesla"
                          , model = 3
                          , name = "The E in SEXY"
                          , travelDirection = Future
                          , price = 35000.00
                          }
       `shouldBe`
       "TimeMachine {manufacturer = \"Tesla\", model = 3, name = \"The E in SEXY\", travelDirection = Future, price = 35000.0}"

  it "countGenders empty" $ do
       countGenders []
       `shouldBe`
       (0, 0, 0)

  it "countGenders one male" $ do
       countGenders [(Individual (Person "A" "B" Male))]
       `shouldBe`
       (1, 0, 0)

  it "countGenders one female" $ do
       countGenders [(Individual (Person "A" "B" Female))]
       `shouldBe`
       (0, 1, 0)

  it "countGenders one unknown" $ do
       countGenders [(Individual (Person "A" "B" Unknown))]
       `shouldBe`
       (0, 0, 1)

  it "countGenders GovOrg" $ do
       countGenders [(GovOrg "A")]
       `shouldBe`
       (0, 0, 0)

  it "countGenders Company" $ do
       countGenders [(Company "A" 1 (Person "A" "B" Male) "B")]
       `shouldBe`
       (0, 0, 0)

  it "countGenders one of each" $ do
       countGenders [ (Individual (Person "A" "B" Male))
                    , (Individual (Person "A" "B" Female))
                    , (Individual (Person "A" "B" Unknown))
                    ]
       `shouldBe`
       (1, 1, 1)

  it "applyPriceMultiplier 50% to two TimeMachines" $ do
       applyPriceMultiplier 0.5 [ TimeMachine { manufacturer = "Tesla"
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
                                ]
       `shouldBe`
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

  it "null' empty" $ do
       null' []
       `shouldBe`
       True

  it "null' one" $ do
       null' [1]
       `shouldBe`
       False

  it "head' one" $ do
       head' [1]
       `shouldBe`
       1

  it "tail' three" $ do
       tail' [1, 2, 3]
       `shouldBe`
       [2, 3]

  it "ackermann 0 0" $ do
       ackermann 0 0
       `shouldBe`
       1

  it "ackermann 0 1" $ do
       ackermann 0 1
       `shouldBe`
       2

  it "ackermann 1 0" $ do
       ackermann 1 0
       `shouldBe`
       2

  it "ackermann 1 1" $ do
       ackermann 1 1
       `shouldBe`
       3

  it "ackermann 0 2" $ do
       ackermann 0 2
       `shouldBe`
       3

  it "ackermann 2 0" $ do
       ackermann 2 0
       `shouldBe`
       3

  it "ackermann 1 2" $ do
       ackermann 1 2
       `shouldBe`
       4

  it "ackermann 2 1" $ do
       ackermann 2 1
       `shouldBe`
       5

  it "ackermann 2 2" $ do
       ackermann 2 2
       `shouldBe`
       7

  it "unzip' empty" $ do
       unzip' [] :: ([Int], [String])
       `shouldBe`
       ([], [])

  it "unzip' one" $ do
       unzip' [(1, "a")]
       `shouldBe`
       ([1], ["a"])

  it "unzip' two" $ do
       unzip' [(1, "a"), (2, "b")]
       `shouldBe`
       ([1, 2], ["a", "b"])

  it "filterOnes" $ do
       filterOnes [1, 4, 2, 1, 2, 3, 6]
       `shouldBe`
       [1, 1]

  it "filterNumber" $ do
       filterNumber 5 [1, 2, 3, 4, 5, 6, 7, 8, 1, 3, 5, 7]
       `shouldBe`
       [5, 5]

  it "filterNot" $ do
       filterNot (\x -> x == 1) [1, 2, 3]
       `shouldBe`
       [2, 3]

  it "filterGovOrgs" $ do
       filterGovOrgs [ (Company "A" 1 (Person "A" "B" Male) "B")
                     , (GovOrg "A")
                     , (Individual (Person "A" "B" Unknown))
                     ]
       `shouldBe`
       [ (GovOrg "A") ]

  it "filter'" $ do
       filter' (\x -> 0 == (mod x 2)) [1, 2, 3, 4, 5, 6]
       `shouldBe`
       [2, 4, 6]

  it "foldr'" $ do
       foldr' (+) 0 [1, 2, 3, 4]
       `shouldBe`
       10

  it "foldl'" $ do
       foldr' (+) 0 [1, 2, 3, 4]
       `shouldBe`
       10

  it "product' empty" $ do
       product' []
       `shouldBe`
       1

  it "product' one" $ do
       product' [7]
       `shouldBe`
       7

  it "product' many" $ do
       product' [1, 2, 3, 4, 5]
       `shouldBe`
       120

  it "minimumClient empty" $ do
       minimumClient []
       `shouldBe`
       Nothing

  it "minimumClient one" $ do
       minimumClient [ (Individual (Person "A" "B" Unknown))
                     ]
       `shouldBe`
       Just (Individual (Person "A" "B" Unknown))

  it "minimumClient three" $ do
       minimumClient [ (Company "ABCDE" 1 (Person "A" "B" Male) "B")
                     , (GovOrg "ABC")
                     , (Individual (Person "A" "BC" Unknown))
                     ]
       `shouldBe`
       Just (GovOrg "ABC")

  it "all' empty" $ do
       all' []
       `shouldBe`
       True

  it "all' one false" $ do
       all' [False]
       `shouldBe`
       False

  it "all' one true" $ do
       all' [True]
       `shouldBe`
       True

  it "all' many true" $ do
       all' [True, True, True, True, True]
       `shouldBe`
       True

  it "all' almost all true" $ do
       all' [True, True, True, False, True]
       `shouldBe`
       False

  it "minimumBy empty" $ do
       minimumBy (\x -> -x) []
       `shouldBe`
       Nothing

  it "minimumBy one" $ do
       minimumBy (\x -> -x) [1]
       `shouldBe`
       Just 1

  it "minimumBy three" $ do
       minimumBy (\x -> -x) [1, 2, 3]
       `shouldBe`
       Just 3

  it "elem' empty" $ do
       elem' 1 []
       `shouldBe`
       False

  it "elem' one match" $ do
       elem' 1 [1]
       `shouldBe`
       True

  it "elem' one" $ do
       elem' 2 [1]
       `shouldBe`
       False

  it "elem' many" $ do
       elem' 4 [1, 2, 3, 4, 5]
       `shouldBe`
       True
