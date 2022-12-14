import           Lib

import           Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = runSolver solve1 solve2


solve1 :: String -> String
solve1 input =
    show $ S.size $ converge (==) $ iterate (simulate walls start) S.empty
    --render walls start $ converge (==) $ iterate (simulate walls start) S.empty
    where
        start = (500,0)
        walls = parse input

solve2 :: String -> String
solve2 input =
    show $ S.size $ converge (==) $ iterate (simulate2 walls start) S.empty
    --render walls start $ converge (==) $ iterate (simulate2 walls start) S.empty
    where
        start = (500,0)
        walls = parse input

simulate :: Set Point ->  Point -> Set Point -> Set Point
simulate walls start sands =
    case step start of
        Just p  -> S.insert p sands
        Nothing -> sands
    where
        step :: Point -> Maybe Point
        step p@(x,y) | y > maxY = Nothing
                     | isVoid (x,y+1) = step (x, y+1)
                     | isVoid (x-1,y+1) = step (x-1, y+1)
                     | isVoid (x+1,y+1) = step (x+1, y+1)
                     | otherwise        = Just p

        isVoid :: Point -> Bool
        isVoid p = not (S.member p walls) && not (S.member p sands)
        maxY = maximum $ snd <$> S.elems walls

simulate2 :: Set Point ->  Point -> Set Point -> Set Point
simulate2 walls start sands =
    case step start of
        Just p  -> S.insert p sands
        Nothing -> sands
    where
        step :: Point -> Maybe Point
        step p@(x,y) | isVoid (x,y+1) = step (x, y+1)
                     | isVoid (x-1,y+1) = step (x-1, y+1)
                     | isVoid (x+1,y+1) = step (x+1, y+1)
                     | otherwise        = Just p

        isVoid :: Point -> Bool
        isVoid p = not (S.member p walls) && not (S.member p sands) && snd p /= (maxY + 2)

        maxY = maximum $ snd <$> S.elems walls

-- parsning

type Point = (Int, Int)

trace :: [Point] -> [Point]
trace xs = concat $ zipWith line xs (drop 1 xs)

line :: Point -> Point -> [Point]
line a@(x1,y1) b@(x2,y2) = takeWhile (/= b) (iterate (`plus` delta) a) ++ [b]
    where delta = (signum (x2-x1), signum (y2-y1))

plus :: Point -> Point -> Point
plus (x,y) (x', y') = (x + x', y + y')


parse :: String -> Set Point
parse input =
     S.fromList $ concatMap (trace . parseLine) (lines input)

parseLine :: String -> [Point]
parseLine line = fmap toPoint $ chunks 3 $ words $ replace ',' ' ' line
    where toPoint (a:b:_) = (read a, read b)
          toPoint xs      = error $ "parse point error: " ++ concat xs

-- render map

render :: Set Point ->  Point -> Set Point -> String
render walls start sands =
    unlines [ [ char (x,y) | x<-[minX..maxX]] | y<-[minY..maxY] ]
    where
        char p | S.member p walls = '#'
               | S.member p sands = '*'
               | p == start =       'S'
               | otherwise        = ' '
        minY = minimum $ snd <$> start:S.elems (S.union walls sands)
        maxY = maximum $ snd <$> start:S.elems (S.union walls sands)
        minX = minimum $ fst <$> start:S.elems (S.union walls sands)
        maxX = maximum $ fst <$> start:S.elems (S.union walls sands)

