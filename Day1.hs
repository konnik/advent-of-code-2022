--
-- Ett litet skal för Advent of Code 2022
--
import           Data.List (sort)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 = show . maximum . map (sum . map toInt) . splitOn "" . lines

solve2 :: String -> String
solve2 = show . sum . take 3 . reverse . sort . map (sum . map toInt) . splitOn "" . lines
