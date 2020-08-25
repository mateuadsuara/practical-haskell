module Store.Client (Client (..), Person (..), Gender (..), clientName, countGenders) where

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male
            | Female
            | Unknown
            deriving Show

clientName client =
  case client of
    GovOrg     name       -> name
    Company    name _ _ _ -> name
    Individual person     ->
      case person of
        Person firstName lastName gender -> firstName ++ " " ++ lastName

countGenders :: [Client] -> (Int, Int, Int)
countGenders [] = (0, 0, 0)
countGenders clients = case client of
  (GovOrg _ )                      -> countGenders rest
  (Company _ _ _ _)                -> countGenders rest
  (Individual (Person _ _ gender)) -> case gender of
    Male    -> (male + 1, female, unknown)
    Female  -> (male, female + 1, unknown)
    Unknown -> (male, female, unknown + 1)
  where rest = tail clients
        client = head clients
        (male, female, unknown) = countGenders rest
