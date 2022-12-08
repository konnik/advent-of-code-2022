import           Data.Char (digitToInt)
import           Data.List (transpose, zip4, zipWith4)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show $ length $ filter any4 $ zip4 (concat left) (concat right) (concat top) (concat bottom)
    where
        left = fmap visibility grid
        right = fmap (reverse . visibility . reverse)  grid
        top = (transpose . fmap visibility . transpose) grid
        bottom = (transpose . fmap (reverse . visibility . reverse) . transpose) grid
        grid = parseInput input
        any4 (a, b, c, d) = a || b || c || d

solve2 :: String -> String
solve2 input =
    show $ maximum $ zipWith4 mul4 (concat left) (concat right) (concat down) (concat up)
    where
        right = fmap score grid
        left = fmap (reverse . score . reverse)  grid
        down = (transpose . fmap score . transpose) grid
        up = (transpose . fmap (reverse . score . reverse) . transpose) grid
        grid = parseInput input
        mul4 a b c d = a * b * c * d

-- score in one direction

score :: [Int] -> [Int]
score []     = []
score (x:xs) = go x xs : score xs
    where
    go _ [] = 0
    go h (x:xs) | x >= h    = 1
                | otherwise = 1 + go h xs


-- visibility in one direction

visibility :: [Int] -> [Bool]
visibility row = go (-1) row
    where
    go _ [] = []
    go h (x:xs) | x <= h    = False : go h xs
                | otherwise = True : go x xs

-- parsing

parseInput :: String -> [[Int]]
parseInput input = fmap digitToInt <$> lines input
