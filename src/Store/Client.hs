{-# LANGUAGE LambdaCase #-}

module Store.Client
  ( Client (..)
  , Person (..)
  , Gender (..)
  , clientName
  , countGenders
  , filterGovOrgs
  , minimumClient
  ) where

import Data.List (foldl')

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person
            deriving (Show, Eq)

data Person = Person String String Gender
            deriving (Show, Eq)

data Gender = Male
            | Female
            | Unknown
            deriving (Show, Eq)

clientName (GovOrg name) = name
clientName (Company name _ _ _) = name
clientName (Individual (Person fN lN _)) = fN ++ " " ++ lN

countGenders :: [Client] -> (Int, Int, Int)
countGenders [] = (0, 0, 0)
countGenders clients = (m + md, f + fd, u + ud)
  where (m, f, u) = countGenders $ tail clients
        (md, fd, ud) = genderDelta $ head clients

genderDelta (Individual (Person _ _ Male)) = (1, 0, 0)
genderDelta (Individual (Person _ _ Female)) = (0, 1, 0)
genderDelta (Individual (Person _ _ Unknown)) = (0, 0, 1)
genderDelta _ = (0, 0, 0)

filterGovOrgs = filter (\case (GovOrg _) -> True
                              _          -> False)

minimumClient :: [Client] -> Maybe Client
minimumClient = foldl' fn Nothing
  where fn Nothing           y = Just y
        fn x@(Just shortest) y =
          if length (clientName y) < length (clientName shortest)
          then Just y
          else x
