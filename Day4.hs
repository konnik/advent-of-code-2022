import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input = show $ length $ filter (uncurry contains) $ parseLine <$>lines input

solve2 :: String -> String
solve2 input = show $ length $ filter (uncurry overlaps) $ parseLine <$>lines input


type Range = (Int, Int)

contains :: Range -> Range -> Bool
contains (a, b) (c, d) = (a<=c && b>=d) || (a>=c && b<=d)

overlaps :: Range -> Range -> Bool
overlaps (a, b) (c, d) = not (d<a || c >b)

parseLine :: String -> (Range, Range)
parseLine line =
    let
        [a,b,c,d] = words $ replace '-' ' ' $ replace ',' ' ' line
    in
       ((read a, read b), (read c, read d))





