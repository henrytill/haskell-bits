module Stack (Stack, empty, isEmpty, push, top, pop) where


newtype Stack a = StackImpl [a] -- opaque!

empty :: Stack a
empty = StackImpl []

isEmpty :: Stack a -> Bool
isEmpty (StackImpl s) = null s

push :: a -> Stack a -> Stack a
push x (StackImpl s) = StackImpl (x:s)

top :: Stack a -> a
top (StackImpl s) = head s

pop :: Stack a -> (a,Stack a)
pop (StackImpl (s:ss)) = (s,StackImpl ss)
