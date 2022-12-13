import           Data.Char (digitToInt)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show  $ sum $ map (fst) $  filter (snd) $ zip [1..] $ fmap (uncurry check) $ fmap (\pair -> (lexer $ pair !! 0, lexer $ pair !! 1)) $ chunks 3 $ lines input
    --show  $ sum $ map (fst) $  filter (snd) $ zip [1..] $ fmap (uncurry check) $ fmap (\pair -> (lexer $ pair !! 0, lexer $ pair !! 1)) $ chunks 3 $ lines testinput
    --show $ skipRest 0 $ drop 1 $ lexer "[23,23,[99]],[32,32,32]"
    --show $ check (lexer "[9]") (lexer "[[8,7,6]]")

    --show (check a b , a, b)
    where
        a = lexer "[99,9]"
        b = lexer "[[8,7,6]]"


solve2 :: String -> String
solve2 input =
    "Lycka till!!!"


testinput = "[1,1,3,1,1]\n\
            \[1,1,5,1,1]\n\
            \\n\
            \[[1],[2,3,4]]\n\
            \[[1],4]\n\
            \\n\
            \[9]\n\
            \[[8,7,6]]\n\
            \\n\
            \[[4,4],4,4]\n\
            \[[4,4],4,4,4]\n\
            \\n\
            \[7,7,7,7]\n\
            \[7,7,7]\n\
            \\n\
            \[]\n\
            \[3]\n\
            \\n\
            \[[[]]]\n\
            \[[]]\n\
            \\n\
            \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
            \[1,[2,[3,[4,[5,6,0]]]],8,9]"

data Token = LB | RB | I Int deriving (Eq, Show)

check :: [Token] -> [Token] -> Bool
check [] []             = True
check [] (_:_)          = True
check (_:_) []          = False
check (LB:as) (LB:bs)   = check as bs
check (RB:as) (RB:bs)   = check as bs
check (RB:as) bs        = check as (skipRest 0 bs)
check as (RB:bs)        = False
check (I a:as) (I b:bs) | a < b      = True
                        | a == b     = check as bs
                        | otherwise  = False
check (I a:as) (LB:bs)  = check (LB:I a:RB:as) (LB:bs)
check (LB:as) (I b:bs)  = check (LB:as) (LB:I b:RB:bs)


skipRest :: Int -> [Token] -> [Token]
skipRest _ []      = []
skipRest n (LB:xs) = skipRest (n+1) xs
skipRest 0 (RB:xs) = xs
skipRest n (RB:xs) = skipRest (n-1) xs
skipRest n (_:xs)  = skipRest n xs


a = lexer "[1,1,3,1,1]"
b = lexer "[1,1,5,1,1]"
x = check a b

lexer :: String -> [Token]
lexer []           = []
lexer ('[':xs)     = LB:lexer xs
lexer (']':xs)     = RB:lexer xs
lexer (',':xs)     = lexer xs
lexer (a:',':xs)   = I (digitToInt a) : lexer xs
lexer (a:']':xs)   = I (digitToInt a) : RB: lexer xs
lexer (a:b:',':xs) = I (digitToInt a * 10 + digitToInt b) : lexer xs
lexer (a:b:']':xs) = I (digitToInt a * 10 + digitToInt b) : RB: lexer xs
lexer xs           = error $ "Invalid input: '" ++ xs ++ "'"


--[[],[[[0],[8,10,2,8],[4]],[[7,7,2,2],10,1,2,[]]],[[[6,1,6,8,10],[8,6,4],[],[],2]]]
--[[[[]],9],[]]


data Packet = PList [Packet] | PInt
