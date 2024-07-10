-- Problem 1 (*) Find the last element of a list.


myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs


-- Usage 
-- myLast [1,2,3,4]
-- myLast ['a', 'b', 'c', 'd']