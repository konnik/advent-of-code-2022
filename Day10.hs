import           Data.List (scanl')
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 =
    show . sum . map signalStrength . filter cycles . run (1,1) . lines
    where
        cycles = (`elem` [20,60..220]) . fst
        signalStrength = uncurry (*)

solve2 :: String -> String
solve2 =
    unlines . chunks 40 . map pixel . run (1,1) . lines

pixel :: (Int, Int) -> Char
pixel (cycle, x) =
    if abs (((cycle-1) `mod` 40)-x) <= 1
    then '#'
    else ' '

run :: (Int, Int) -> [String] -> [(Int, Int)]
run _ [] = []
run (cycle, val) (x:xs)  =
    case words x of
        ["noop"]    -> (cycle, val):run (cycle+1,val) xs
        ["addx", x] -> (cycle, val):(cycle+1,val):run (cycle+2, val + read x) xs
        x           -> error ("Illegal instruction: "  ++ concat x)
