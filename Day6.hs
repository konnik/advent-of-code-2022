import           Data.List (nub, sort)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 = show . (+4) . length . takeWhile (not . marker) . slidingWindow 4

solve2 :: String -> String
solve2 = show . (+14) . length . takeWhile (not . marker) . slidingWindow 14

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs | length xs < n = []
                   | otherwise     = take n xs : slidingWindow n (drop 1 xs)

marker :: Ord a => [a] -> Bool
marker xs = xs == nub xs

