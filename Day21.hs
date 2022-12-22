import           Data.Map (Map, fromList, (!))
import qualified Data.Map as M
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show $ flip resolve "root" $ parseInput input

solve2 :: String -> String
solve2 input =
    "Lycka till!!!"


resolve :: Map String Expr -> String -> Maybe Int
resolve m name   = do
    expr <- M.lookup name m
    case expr of
        Val x   -> Just x
        Add a b -> (+) <$> resolve m a  <*> resolve m b
        Sub a b -> (-) <$> resolve m a  <*> resolve m b
        Mul a b -> (*) <$> resolve m a  <*> resolve m b
        Div a b -> div <$> resolve m a  <*> resolve m b

data Expr
    = Val Int
    | Add String String
    | Sub String String
    | Mul String String
    | Div String String
    deriving (Show )

testinput = "root: pppw + sjmn\n\
    \dbpl: 5\n\
    \cczh: sllz + lgvd\n\
    \zczc: 2\n\
    \ptdq: humn - dvpt\n\
    \dvpt: 3\n\
    \lfqf: 4\n\
    \humn: 5\n\
    \ljgn: 2\n\
    \sjmn: drzm * dbpl\n\
    \sllz: 4\n\
    \pppw: cczh / lfqf\n\
    \lgvd: ljgn * ptdq\n\
    \drzm: hmdt - zczc\n\
    \hmdt: 32\n"


parseInput :: String -> Map String Expr
parseInput input = fromList $ parseLine . words. replace ':' ' ' <$> lines input

parseLine :: [String] -> (String, Expr)
parseLine [name, x]         = (name, Val (read x))
parseLine [name, a, "+", b] = (name, Add a b)
parseLine [name, a, "-", b] = (name, Sub a b)
parseLine [name, a, "*", b] = (name, Mul a b)
parseLine [name, a, "/", b] = (name, Div a b)
parseLine _                 = error "Invalid line"

