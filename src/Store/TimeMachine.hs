module Store.TimeMachine (TimeMachine (..), Manufacturer (..), Model (..), Name (..), TravelDirection (..), Price (..), applyPriceMultiplier) where

data TimeMachine = TimeMachine { manufacturer :: Manufacturer
                               , model :: Model
                               , name :: Name
                               , travelDirection :: TravelDirection
                               , price :: Price
                               }
                 deriving (Show, Eq)

type Manufacturer = String

type Model = Int

type Name = String

data TravelDirection = Past
                     | Future
                     deriving (Show, Eq)

type Price = Float

applyPriceMultiplier :: Float -> [TimeMachine] -> [TimeMachine]
applyPriceMultiplier multiplier timeMachines = map (applyPriceMultiplierForOne multiplier) timeMachines

applyPriceMultiplierForOne :: Float -> TimeMachine -> TimeMachine
applyPriceMultiplierForOne multiplier t@(TimeMachine { price = p }) =
  t { price = p * multiplier }
