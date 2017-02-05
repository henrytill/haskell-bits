-- Quicksort in Haskell (after Hudak)

qs []     = []
qs (x:xs) = qs [y | y<-xs, y<x ] ++ [x] ++
            qs [y | y<-xs, y>=x]
