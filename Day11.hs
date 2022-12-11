import           Data.List (nub, sort)
import           Lib
import           Prelude   hiding (round)

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show $ solve 20 limitByDiv monkeys
    where
        limitByDiv = (`div` 3)
        monkeys = parse input

solve2 :: String -> String
solve2 input =
    show $ solve 10000 limitByMod monkeys
    where
        limitByMod x = x `mod` product (nub $ divisibleBy <$> monkeys)
        monkeys = parse input


solve :: Int -> (Int -> Int) -> [Monkey] -> Int
solve rounds f =
    product . take 2 . reverse . sort . fmap count . (!! rounds) . iterate (round f)

round :: (Int -> Int) -> [Monkey] -> [Monkey]
round f ms = iterate (oneTurn f) ms !! length ms


oneTurn :: (Int -> Int) -> [Monkey] -> [Monkey]
oneTurn f [] = []
oneTurn f (m:ms) =  (distribute thrownItems <$> ms) ++ [distribute thrownItems m']
    where
        (m', thrownItems) = inspectItems f m

distribute :: [(Int, Int)] -> Monkey -> Monkey
distribute thrownItems toMonkey = toMonkey { items = items toMonkey ++ newItems }
    where
        newItems = snd <$> filter ((==monkeyId toMonkey) . fst) thrownItems

inspectItems :: (Int -> Int) -> Monkey -> (Monkey, [(Int, Int)])
inspectItems f m =
            ( m { items = []
                , count = count m + length (items m)
                }
            , inspectOne f m <$> items m
            )

inspectOne :: (Int -> Int) -> Monkey -> Int -> (Int, Int)
inspectOne f m x = (toMonkey, x')
    where
        x' = f (op m x)
        toMonkey = if test then trueMonkey m else falseMonkey m
        test = x' `mod` divisibleBy m == 0


data Monkey = Monkey
    { monkeyId    :: Int
    , items       :: [Int]
    , op          :: Int -> Int
    , divisibleBy :: Int
    , trueMonkey  :: Int
    , falseMonkey :: Int
    , count       :: Int
    }


parse :: String -> [Monkey]
parse input = map parseMonkey $ chunks 7 $ lines input
    where
    parseMonkey :: [String] -> Monkey
    parseMonkey xs = Monkey
        { monkeyId =  read $ words (replace ':' ' ' (xs !! 0)) !! 1
        , items = fmap read $ drop 2 $ words $ replace ',' ' ' $ xs !! 1
        , op = case drop 3 $ words $ xs !! 2 of
                    ["old", "*", "old"] -> (\old -> old*old)
                    ["old", "*", b    ] -> (* read b)
                    ["old", "+", b    ] -> (+ read b)
                    _                   -> error "Invalid op"
        , divisibleBy = read (words (xs !! 3) !! 3)
        , trueMonkey = read $ words (xs !! 4) !! 5
        , falseMonkey = read $ words (xs !! 5) !! 5
        , count = 0
        }
