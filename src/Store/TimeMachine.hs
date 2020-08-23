module Store.TimeMachine (TimeMachine (..), Manufacturer (..), Model (..), Name (..), TravelDirection (..), Price (..)) where

data TimeMachine = TimeMachine Manufacturer Model Name TravelDirection Price
                 deriving Show

data Manufacturer = Manufacturer String
                  deriving Show

data Model = Model Integer
           deriving Show

data Name = Name String
          deriving Show

data TravelDirection = Past
                     | Future
                     deriving Show

data Price = Price Float
           deriving Show
