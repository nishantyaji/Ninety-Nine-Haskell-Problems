-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.

-- I could not do this

-- Copied the following approaches 

split::[a] -> Int -> ([a],[a])
split xs 0 = ([], xs)
split [] _ = ([], [])
split (x:xs) n = let (first, second) = split xs (n-1) in (x:first, second)

split'::[a] -> Int -> ([a],[a])
split' xs n = (take n xs, drop n xs)

split'''::[a] -> Int -> ([a],[a])
split''' xs n = splitAcc xs n []

splitAcc::[a] -> Int -> [a] -> ([a],[a])
splitAcc xs 0 acc = (reverse acc, xs)
splitAcc [] _ acc  = (reverse acc, [])
splitAcc (x:xs) n acc = splitAcc xs (n-1) (x:acc)
