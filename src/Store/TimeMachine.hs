module Store.TimeMachine (TimeMachine (..), Manufacturer (..), Model (..), Name (..), TravelDirection (..), Price (..), applyPriceMultiplier) where

data TimeMachine = TimeMachine { manufacturer :: Manufacturer
                               , model :: Model
                               , name :: Name
                               , travelDirection :: TravelDirection
                               , price :: Price
                               }
                 deriving (Show, Eq)

type Manufacturer = String

data Model = Model Integer
           deriving (Show, Eq)

data Name = Name String
          deriving (Show, Eq)

data TravelDirection = Past
                     | Future
                     deriving (Show, Eq)

data Price = Price Float
           deriving (Show, Eq)

applyPriceMultiplier :: Float -> [TimeMachine] -> [TimeMachine]
applyPriceMultiplier multiplier timeMachines = map (applyPriceMultiplierForOne multiplier) timeMachines

applyPriceMultiplierForOne :: Float -> TimeMachine -> TimeMachine
applyPriceMultiplierForOne multiplier (TimeMachine manufacturer model name travelDirection (Price price)) =
  TimeMachine manufacturer model name travelDirection (Price $ price * multiplier)
