module Stack
  ( Stack
  , empty
  , isEmpty
  , push
  , top
  , pop
  ) where

newtype Stack a = Stack [a] -- opaque!

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack s) = null s

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x:s)

top :: Stack a -> a
top (Stack s) = head s

pop :: Stack a -> (a, Stack a)
pop (Stack (s:ss)) = (s, Stack ss)
