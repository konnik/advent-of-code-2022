{-# LANGUAGE LambdaCase #-}

import           Data.List (sort)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input = show $ sum $ scoreLine <$> fmap parseLine (lines input)

solve2 :: String -> String
solve2 input = show $ sum $ scoreLine <$> fmap parseLine2 (lines input)


data Hand = Rock | Paper | Scissor deriving Eq
data Outcome = Win | Draw | Loss


scoreLine :: (Hand, Hand) -> Int
scoreLine (a, b) = scoreOutcome (outcome a b) + scoreHand b

scoreHand :: Hand -> Int
scoreHand = \case
    Rock    -> 1
    Paper   -> 2
    Scissor -> 3

scoreOutcome :: Outcome -> Int
scoreOutcome = \ case
    Win  -> 6
    Draw -> 3
    Loss -> 0


outcome :: Hand -> Hand -> Outcome
outcome opponent me | win opponent == me    = Win
                    | loose opponent == me  = Loss
                    | otherwise             = Draw


myHand :: Char -> Hand
myHand = \case
    'Y' -> Paper
    'X' -> Rock
    'Z' -> Scissor
    _   -> error "wrong input for my hand"

opponentHand :: Char -> Hand
opponentHand = \case
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissor
    _   -> error "wrong input fpr opponent hand"

myStrategy :: Char -> (Hand -> Hand)
myStrategy = \ case
    'X' -> loose
    'Y' -> draw
    'Z' -> win
    _   -> error "wrong input for my stategy"

loose :: Hand -> Hand
loose to = dropWhile (to /= ) (cycle [Paper, Rock, Scissor]) !! 1

win :: Hand -> Hand
win over = dropWhile (over /= ) (cycle [Scissor, Rock, Paper]) !! 1

draw :: Hand -> Hand
draw = id

-- parsing

parseLine :: String -> (Hand, Hand)
parseLine [a,_,b] = (opponentHand a, myHand b)
parseLine _       = error "error line"

parseLine2 :: String -> (Hand, Hand)
parseLine2 [a,_,b] = (opponentHand a, myStrategy b (opponentHand a))
parseLine2 _       = error "error line"


