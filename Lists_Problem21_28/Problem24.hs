-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.

-- cabal update
-- cabal install --lib random

import System.Random (randomRIO)

-- Function to generate a random number between 1 and a given upper limit
-- Note the left arrow (<-) symbol for IO based input
-- Note the if then else statement that seems to handle the <- case
generateRandomNumber :: Int -> [Int] -> IO Int
generateRandomNumber l ls = do
  num <- randomRIO (1, l)
  if num `elem` ls then generateRandomNumber l ls else return num

-- Function to generate a list of 'n' random numbers between 1 and a given upper limit
getRand :: Int -> Int -> IO [Int]
getRand 0 _ = return []
getRand n l = do
  nums <- getRand (n - 1) l
  num <- generateRandomNumber l nums
  return (num : nums)

-- Function to get the character at the given index in a list
charAt :: (Eq a) => [a] -> Int -> a
charAt [] _ = error "Index out of bounds"
charAt [x] 1 = x
charAt (x : xs) 1 = x
charAt (x : xs) n = charAt xs (n - 1)

-- Function to get characters at multiple indices in a list
charAtMany :: (Eq a) => [a] -> [Int] -> [a]
charAtMany _ [] = []
charAtMany xs (r : rands) = (charAt xs r) : charAtMany xs rands

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x : xs)
  | x `elem` xs = uniq xs
  | otherwise = x : uniq xs

-- uniq (x:xs) = if x `elem` xs then uniq xs else x
-- uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Function to select 'n' random elements from a list
-- do block because it uses IO/monad
diff_select :: (Eq a) => [a] -> Int -> IO [a]
diff_select xs n = do
  indices <- getRand n (length xs)
  return (charAtMany xs indices)

-- Example usage
-- diff_select "abcdefghik" 3
main :: IO ()
main = do
  result <- diff_select "abcdefghik" 3
  putStrLn $ "Randomly selected elements: " ++ show result
