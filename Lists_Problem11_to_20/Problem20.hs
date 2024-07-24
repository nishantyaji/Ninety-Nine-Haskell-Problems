-- Problem 20
-- (*) Remove the K'th element from a list.

charAtSp :: (Eq a) => [a] -> Int -> Int -> a
charAtSp [] _ _ = error "Index out of bounds"
charAtSp (x : xs) p q
  | p == q = x
  | otherwise = charAtSp xs (p + 1) q

charAt :: (Eq a) => [a] -> Int -> a
charAt [] _ = error "Index out of bounds"
charAt [x] 1 = x
charAt (x : xs) 1 = x
charAt (x : xs) n = charAt xs (n - 1)

removeAt' :: (Eq a) => Int -> [a] -> (a, [a])
removeAt' _ [] = error "Empty List"
removeAt' n (x : xs) = (charAt (x : xs) n, take (n - 1) (x : xs) ++ drop n (x : xs))

-- following alternatives found on github, not mine

-- removeAt 1 (x:xs) = (x,xs)
-- removeAt i (x:xs) = let (rem,rest) = removeAt (i-1) xs in (rem, x:rest)

-- removeAt :: [a] -> Int -> ([a],a)
-- removeAt list k = let (first, second) = splitAt k list
--                   in ((init first) ++ second, last first)

-- Usage removeAt 2 "abcd"
-- ('b',"acd")
