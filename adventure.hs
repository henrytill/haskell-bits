{-# LANGUAGE PolyKinds #-}

-- data Maybe a = Nothing | Just a

data T f a = MkT (f a)
type T1 = T Maybe Int

data F f = MkF (f Int)
type T2 = T F Maybe

main :: IO ()
main = do
    putStrLn "Hello, world!"
