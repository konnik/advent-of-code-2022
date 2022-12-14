import           Data.List (sortBy)
import           Lib
import           Maybes    (fromJust)

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    show  $ sum $ findIndices (==LT) $ fmap (uncurry comparePackets . parseChunk) $ chunks 2 $ parse input
    where
        parseChunk pair = (head pair, pair !! 1)


solve2 :: String -> String
solve2 input =
        show $ product $ findIndices (`elem` [divider1, divider2] ) $ sortBy comparePackets packets
    where
        packets = divider1:divider2:parse input
        divider1 = head $ parse "[[2]]"
        divider2 = head $ parse "[[6]]"


data Packet
    = List [Packet]
    | Int Int
    deriving (Show, Eq)

-- comparing

comparePackets :: Packet -> Packet -> Ordering
comparePackets (Int a) (Int b)
                       | a < b     = LT
                       | a == b    = EQ
                       | otherwise = GT
comparePackets (List as) (List bs) =  compareList as bs
comparePackets (Int a)   (List bs) =  compareList [Int a] bs
comparePackets (List as) (Int b)   =  compareList as [Int b]

compareList :: [Packet] -> [Packet] -> Ordering
compareList []     []     = EQ
compareList []     (_:_)  = LT
compareList (_:_)  []     = GT
compareList (a:as) (b:bs) = case comparePackets a b of
                            EQ -> compareList as bs
                            x  -> x

-- parsning

parse :: String -> [Packet]
parse = fmap (fst . fromJust . parseValue . tokenize) . filter (/="") . lines

parseList :: [Token] -> Maybe (Packet, [Token])
parseList [] = error "no more tokens"
parseList (LB:xs) =
    case parseValues xs of
        Just (vals, xs') -> Just (List vals, xs')
        Nothing          -> Nothing

parseList (_:xs) = Nothing

parseValues :: [Token] -> Maybe ([Packet], [Token])
parseValues (RB:xs) = Just ([], xs)
parseValues xs =
    case parseValue xs of
        Nothing -> error "expected value"
        Just (val, xs') ->
            case parseValues xs' of
                Just (vals, xs'') -> Just (val:vals, xs'')
                Nothing           -> Nothing

parseValue :: [Token] -> Maybe (Packet, [Token])
parseValue (LB:xs)  = parseList (LB:xs)
parseValue (I n:xs) = Just (Int n, xs)
parseValue _        = Nothing

-- tokenize

data Token = LB | RB | I Int deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize []           = []
tokenize ('[':xs)     = LB:tokenize xs
tokenize (']':xs)     = RB:tokenize xs
tokenize (',':xs)     = tokenize xs
tokenize (a:',':xs)   = I (read [a]) : tokenize xs
tokenize (a:']':xs)   = I (read[a]) : RB: tokenize xs
tokenize (a:b:',':xs) = I (read [a,b]) : tokenize xs
tokenize (a:b:']':xs) = I (read [a,b]) : RB: tokenize xs
tokenize xs           = error $ "Invalid input: '" ++ xs ++ "'"


