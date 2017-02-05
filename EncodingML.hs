{-# LANGUAGE TypeFamilies #-}

module EncodingML where

newtype Positive a = Positive a

ze :: Positive Int
ze = Positive 0

su :: Positive Int -> Positive Int
su (Positive x) = Positive (x + 1)

toInt :: Positive Int -> Int
toInt (Positive x) = x
