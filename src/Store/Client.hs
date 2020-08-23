module Store.Client (Client (..), Person (..), Gender (..), clientName) where

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
