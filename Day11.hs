import           Data.List (nub, sort)
import           Lib
import           Prelude   hiding (round)

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show $ product $ take 2 $ reverse $ sort $ fmap count $ head $ drop 20 $ iterate (round (`div` 3)) $ parse input
   -- show $ fmap count $ head $ drop 20 $ iterate round $ parse input

solve2 :: String -> String
solve2 input =
    --show $ product $ take 2 $ reverse $ sort $ fmap count $ head $ drop 1 $ iterate round $ parse input

    show $ product $ take 2 $ reverse $ sort $ fmap count $ head $ drop 10000 $ iterate (round f)  $ parse input

    where
        f x = x `mod` factors
        factors = product $ nub $ fmap divisibleBy $ parse input

round :: (Int -> Int) -> [Monkey] -> [Monkey]
round f ms = iterate (oneTurn f) ms !! (length ms)


oneTurn :: (Int -> Int) -> [Monkey] -> [Monkey]
oneTurn f [] = []
oneTurn f (m:ms) =  (distribute thrownItems <$> ms) ++ [distribute thrownItems m']
    where
        (m', thrownItems) = inspectItems f m

distribute :: [(Int, Int)] -> Monkey -> Monkey
distribute xs m = m { items = items m ++ newItems }
    where
        newItems = fmap snd $ filter ((==monkeyId m ). fst) xs

inspectItems :: (Int -> Int) -> Monkey -> (Monkey, [(Int, Int)])
inspectItems f m =  (m{ items = [], count = count m + length (items m)}, inspect f m <$> items m)

inspect :: (Int -> Int) -> Monkey -> Int -> (Int, Int)
inspect f m x = (toMonkey, x')
    where
        x' = f (op m x)
        toMonkey = if test x' then trueMonkey m else falseMonkey m
        test a = a `mod` divisibleBy m == 0


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

parseMonkey :: [String] -> Monkey
parseMonkey xs = Monkey id items op divisibleBy trueM falseM 0
    where
        id =  read $ words (replace ':' ' ' (xs !! 0)) !! 1
        items = fmap read $ drop 2 $ words $ replace ',' ' ' $ xs !! 1
        op = case drop 4 $ words $ xs !! 2 of
                ["*","old"] -> (\old -> old*old)
                ["*",b]     -> (* read b)
                ["+",b]     -> (+ read b)
                _           -> error "Invalid op"
        divisibleBy = read (words (xs !! 3) !! 3)
        trueM = read $ words (xs !! 4) !! 5
        falseM = read $ words (xs !! 5) !! 5
