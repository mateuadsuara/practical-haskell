module Chapter2.SimpleFunctions (firstOrEmpty) where

firstOrEmpty lst = if not (null lst) then head lst else "empty"
