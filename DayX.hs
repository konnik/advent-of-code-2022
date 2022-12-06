import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    "Antal rader i input: " ++ show ((length . lines) input)

solve2 :: String -> String
solve2 input =
    "Lycka till!!!"
