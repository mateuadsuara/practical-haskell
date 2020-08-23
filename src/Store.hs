module Store (Client) where

data Client = GovOrg     String
            | Company    String Integer String String
            | Individual String String Bool
            deriving Show
