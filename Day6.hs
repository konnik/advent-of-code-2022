import           Data.List (nub, sort)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 = show . (+4) . length . takeWhile (not . unique) . slidingWindow 4

solve2 :: String -> String
solve2 = show . (+14) . length . takeWhile (not . unique) . slidingWindow 14

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs = take n xs : slidingWindow n (drop 1 xs)

unique :: Ord a => [a] -> Bool
unique xs = length xs == length (nub $ sort xs)

