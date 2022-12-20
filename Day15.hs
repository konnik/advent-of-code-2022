import           Data.List (nub, sortBy, sortOn)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show $ lenOfSlices-numBeacons
    where
        pairs = parse input
        lenOfSlices = sum $ len <$> slicesAndBeacons pairs 2000000
        numBeacons = countBeaconsAt 2000000 pairs

solve2 :: String -> String
solve2 input =
    show $ answer $ head $ filter ((/=1).length.snd) $ zip [0..limit] $ fmap (slicesAndBeacons pairs) [0..limit]
    where
        answer (y,[Range _ a, Range _ _]) = y + limit*(a+1)
        answer _                          = error "Invalid result"
        limit = 4000000
        pairs = parse input


slicesAndBeacons :: [Pair] -> Int -> [Range]
slicesAndBeacons pairs y = merge $ sortOn startofRange (allSlices pairs y ++ fmap pointAsRange (beaconsAt y pairs))

allSlices :: [Pair] -> Int -> [Range]
allSlices pairs y = concatMap (slice y) pairs

pointAsRange :: Point -> Range
pointAsRange (x,y) = Range x x

beaconsAt :: Int -> [Pair] -> [Point]
beaconsAt y pairs = nub $ filter ((==y) . snd ) $ snd <$> pairs

countBeaconsAt :: Int -> [Pair] -> Int
countBeaconsAt y pairs =length $ beaconsAt y pairs


-- range data type and operations

data Range = Range Int Int deriving (Show)

merge :: [Range] -> [Range]
merge [] = []
merge [x] = [x]
merge ((Range a b):(Range c d):xs)
    | a>c = error "Must be sorted"
    | c<=b+1  = merge (Range a (max b d):xs)
    | otherwise = Range a b:merge (Range c d:xs)

startofRange :: Range -> Int
startofRange (Range a _) = a

len :: Range -> Int
len (Range a b) = b-a+1

slice :: Int -> Pair -> [Range]
slice y ((sx, sy), (bx, by))
    | off >= 0 = [Range (sx - off) (sx + off)]
    | otherwise = []
    where
        a = sx
        off = m-abs(y-sy)
        m = abs (sx-bx) + abs (sy-by)


type Pair = (Point, Point)
type Point = (Int, Int)


parse :: String -> [Pair]
parse = fmap parseLine . lines

parseLine :: String -> Pair
parseLine xs = ((read $ x !! 3, read $ x !!5), (read $ x !! 11, read $ x !!13))
    where x = words $ replaceAll ",:=" ' ' xs

