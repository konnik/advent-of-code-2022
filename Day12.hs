import           Data.Char (ord)
import           Data.List (sort, transpose)
import           Lib

main :: IO ()
main = runSolver solve1 solve2


solve1 :: String -> String
solve1 input =
    show $ steps $ cellAt start $ solve input
    where
        start = startPos input


solve2 :: String -> String
solve2 input =
    show $ minimum (fmap steps $ cellsWithHeight 0 $ solve input)


solve :: String -> Heightmap
solve input =
    converge (==) $ iterate floodFill $ parseMap input

-- Heightmap stuff

type Pos = (Int,Int)
type Heightmap = [[Cell]]
data Cell = Cell { height::Int, steps :: Int } deriving (Eq, Show)


floodFill :: Heightmap -> Heightmap
floodFill  =  d . u . l . r
    where
        r = fmap fillRow
        l = fmap (reverse . fillRow . reverse)
        d = transpose . fmap fillRow . transpose
        u = transpose . fmap (reverse . fillRow . reverse) . transpose

fillRow :: [Cell] -> [Cell]
fillRow = scanl1 fillNeighbour

fillNeighbour :: Cell -> Cell -> Cell
fillNeighbour a b
    | (height b - height a >= -1) && steps a < steps b    = b { steps = steps a + 1 }
    | otherwise                       = b


cellAt :: Pos -> Heightmap -> Cell
cellAt p = (!!fst p ) . (!!snd p)

cellsWithHeight :: Int -> Heightmap -> [Cell]
cellsWithHeight h hmap = [ c | (l, col) <- zip hmap [0..], (c, row) <- zip l [0..], height c == h ]

-- parsning

parseMap :: String -> Heightmap
parseMap input = fmap item <$> lines input
    where
        item ch =
            case ch of
                'S' -> Cell 0 9999999999
                'E' -> Cell 25 0
                x   -> Cell (ord x - ord 'a') 9999999999


startPos :: String -> Pos
startPos input = head [ (row, col) | (l, col) <- zip (lines input) [0..], (c, row) <- zip l [0..], c=='S' ]
