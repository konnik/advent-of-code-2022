--
-- Ett litet skal för Advent of Code 2022
--
import           Data.Char (ord)
import           Data.List (intersect, nub)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input = show $ sum $ fmap (prio . head . uncurry intersect) $ fmap dedup $ halves <$> lines input

solve2 :: String -> String
solve2 input = show $ sum $ fmap prio $ badges $ lines input


dedup :: (String, String) -> (String, String)
dedup (a, b) = (nub a, nub b)

halves :: String -> (String, String)
halves str = splitAt half str
    where half = length str `div` 2

badges :: [String] -> [Char]
badges []           = []
badges (a:b:c:rest) = head (nub a `intersect` (nub b `intersect` nub c)):badges rest
badges _ = error "wrong input"



prio :: Char -> Int
prio ch | ch >= 'a'     = ord ch - ord 'a' + 1
        | otherwise     = ord ch - ord 'A' + 27
