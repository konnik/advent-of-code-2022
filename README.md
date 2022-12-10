# Advent of Code 2022 in Haskell

Trying to solve as many problems as possible using Haskell using only functionality from 
[base](https://hackage.haskell.org/package/base).

My goal is mainly to solve the problems and not necessary write the most beautiful Haskell code. :-)

## File structure

Source code and the input for each day is saved in two files. Example for day 1:

* `Day1.hs`
* `Day1.txt`

Common functions i would like to reuse between days I'll put in `Lib.hs`.

## Structure of each days source code

Every day has it's own source file with its own `main`-funktion. The `main` function is just delegating to `Lib.runSolver` which uses the function [interact](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:interact) and does all IO for your program (reads from `stdin` and writes to `stdout`). All I have to do is implement the two functions `solve1` and `solve2` with signature `String -> String` (the solutions for part 1 and part 2).

```haskell
main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    "<not solved yet>"

solve2 :: String -> String
solve2 input =
    "<not solved yet>"
```

## Initialize a new day

The helper script `init` creates a new empty source file for the given day and also creates an empty file for the input data. 

Example:

```bash
./init <day>
```
**Bonus:** if you create a file `.aoc-session` containing the value of your personal session cookie from adventofcode.com todays input data will be downloaded automatically. 

You can find the value of the session cookie easily using the developer console in your web browser.


## Run a soultion

Run the solution for a given day with the script `watch`. Ecample:

```bash
./watch <day>
```

The script is using the excellent tool [entr](https://github.com/eradman/entr) to automatically compile and run the source code one every code change. 

If everying works out the answers will be displayed:

```bash
Answer 1:
1337

Answer 2:
<not solved yet>
```

Otherwise the compiler error message will be shown:  

```bash 
Day0.hs:11:5: error:
    • Variable not in scope: shov :: Int -> String
    • Perhaps you meant ‘show’ (imported from Prelude)
   |
11 |     shov $ length $ concatMap words $ filter (not . isComment) $ lines input
   |     ^^^^
```

Perfect to run in a terminal on a second screen to get instant feedback on your progress.


## Start the repl

You can start `ghci` with the code for a given day using the script `repl`:

```bash
./repl <day>

GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( Lib.hs, interpreted )
[2 of 2] Compiling Main             ( Day5.hs, interpreted )
Ok, two modules loaded.
*Main> _
```
