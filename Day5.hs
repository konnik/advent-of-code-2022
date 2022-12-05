import           Data.Char (isDigit, isSpace)
import           Data.List (transpose)
import           Lib       (foldl', runSolver, splitOn)

main :: IO ()
main = runSolver solve1 solve2


solve1 :: String -> String
solve1 input = head <$> foldl' (doMove reverse) (parseStacks input) (parseMoves input)

solve2 :: String -> String
solve2 input = head <$> foldl' (doMove id) (parseStacks input) (parseMoves input)


type Stack = [Char]
type Move = (Int, Int, Int)


doMove :: (Stack -> Stack) -> [Stack] -> Move -> [Stack]
doMove op stacks (n, fromId, toId) = (moveTo . moveFrom) stacks
    where
        moveFrom = updateStackWithId fromId $ drop n (stacks !! (fromId-1))
        moveTo   = updateStackWithId toId   $ op (take n (stacks !! (fromId-1))) ++ (stacks !! (toId-1))

updateStackWithId :: Int -> Stack -> [Stack] -> [Stack]
updateStackWithId id newStack stacks = take (id-1) stacks ++ [newStack] ++ drop id stacks

-- parsing

parseStacks :: String -> [Stack]
parseStacks = map (dropWhile isSpace) . filter (isDigit . last) . transpose . takeWhile (/="") . lines

parseMoves :: String -> [Move]
parseMoves = fmap (parse . splitOn ' ') . drop 1 . dropWhile (/="") . lines
    where
        parse [_,a,_,b,_,c] = (read a, read b, read c)
        parse _             = error "wrong move input"
