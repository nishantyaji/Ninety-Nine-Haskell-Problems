-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists.

-- simple implementation of takeWhile
-- Usage
-- takeWhile (<3) [1,2,3,4,5,6,7,8,9]
-- returns = [1,2]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' fn (x : xs)
  | fn x == True = x : takeWhile' fn xs
  | otherwise = []

-- simple implementation of dropWhile
-- Usage
-- takeWhile (<3) [1,2,3,4,5,6,7,8,9]
-- returns = [3, 4, 5, 6, 7, 8, 9]
-- takeWhile (>3) [1,2,3,4,5,6,7,8,9]
-- returns [1,2,3,4,5,6,7,8,9]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' fn (x : xs)
  | fn x == True = dropWhile' fn xs
  | otherwise = x : xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x : xs) = (x : takeWhile' (== x) (xs)) : pack (dropWhile' (== x) xs)

-- Usage
--  pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- output
-- ["aaaa","b","cc","aa","d","eeee"]
