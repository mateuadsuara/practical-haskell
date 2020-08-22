module Main where

import qualified Chapter2 (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Chapter2.someFunc
