import           Data.List (nub)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 = show . solve 2

solve2 :: String -> String
solve2 = show . solve 10

solve :: Int -> String -> Int
solve n input = length $ nub $ fmap last $ scanl moveRope (replicate n (0,0)) $ parse input

type Move = String
type Pos = (Int, Int)

moveRope :: [Pos] -> Move -> [Pos]
moveRope [] _ = error "no head"
moveRope ((hx,hy):knots) dir = adjustKnots (newHead:knots)
    where
    newHead = case dir of
        "U" -> (hx, hy+1)
        "D" -> (hx, hy-1)
        "L" -> (hx-1, hy)
        "R" -> (hx+1, hy)
        m   -> error $ "illegal move" ++ m



adjustKnots :: [Pos] -> [Pos]
adjustKnots [] = []
adjustKnots [a] = [a]
adjustKnots x@(h@(hx, hy):(tx, ty):tails)
    | abs dx == abs dy && d <=2 = x -- touching corners or overlapping
    | d == 1                    = x -- touching sides
    | otherwise                 = h:adjustKnots ((tx + signum dx, ty + signum dy):tails)
    where
        dx = hx-tx
        dy = hy-ty
        d = abs dx + abs dy


parse :: String -> [Move]
parse = concatMap (\[a,b] -> replicate (read b) a) . fmap words . lines
