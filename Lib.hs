
module Lib
    ( module Lib
    , module Data.Foldable )
where

import           Data.Foldable (foldl', foldr)

-- MAIN RUNNER

runSolver :: (String -> String) -> (String -> String) -> IO ()
runSolver s1 s2 =
    interact $ solveBoth s1 s2
    where
        solveBoth :: (String -> String) -> (String -> String) -> String -> String
        solveBoth s1 s2 input =
            "Answer 1:\n" ++ s1 input ++ "\n\n" ++ "Answer 2:\n" ++ s2 input ++ "\n"

toInt :: String -> Int
toInt = read

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep [] = []
splitOn sep xs = takeWhile (/=sep) xs : splitOn sep (drop 1 $ dropWhile (/=sep) xs)


converge :: (a -> a -> Bool) -> [a] -> a
converge f xs = fst $ head $ dropWhile (not . uncurry f) $ zip xs $ drop 1 xs

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices f = fmap fst . filter (f . snd) . zip [1..]


-- | replace all elements of value a with value b
replace :: Eq a => a -> a -> [a] -> [a]
replace a b = fmap (\x -> if x == a then b else x)


replaceAll :: Eq a => [a] -> a -> [a] -> [a]
replaceAll as b = fmap (\x -> if x `elem` as then b else x)

-- | Replace an element at a specific index with a new value.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newValue xs | n < 0          = xs
                        | n >= length xs = xs
                        | otherwise      = take n xs ++ [newValue] ++ drop (n+1) xs

-- | Update an element at a specific index with a new value.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs | n < 0          = xs
                | n >= length xs = xs
                | otherwise      = take n xs ++ [f (xs !! n)] ++ drop (n+1) xs


chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
