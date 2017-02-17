module Temp where

k :: Either String String -> IO ()
k x = putStrLn (show x)

right = k . Right
left  = k . Left
