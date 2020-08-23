module Store (Client) where

data Client = GovOrg     String
            | Company    String Integer String String
            | Individual Person
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male
            | Female
            | Unknown
            deriving Show
