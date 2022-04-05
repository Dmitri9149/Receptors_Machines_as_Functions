module Stack where

type Stack a = [a]
pop :: Stack a -> (a,Stack a)
pop (x:xs) = (x,xs)
pop [] = error "pop from empty stack"
push :: a -> Stack a -> ((),Stack a)
push a xs = ((),a:xs)