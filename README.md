# Advent of Code 2022 i Haskell

Ett försök att lösa så många uppgifter som möjligt i årets Advent of Code med Haskell.

## Filstruktur

Källkod och input för dag `1` sparas som två filer:

* `Day1.hs`
* `Day1.txt`

Smarta funktioner som man vill kunna återanvända mellan olika dagar läggs i `Lib.hs`.


## Programstruktur

Varje dag har sin egen källkodsfil med en `main`-funktion. Main använder sig av standardfunktionen [interact](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:interact) som sköter all io åt dig (läser från `stdin` och skriver till `stdout`) och du behöver bara implementera funktionerna `solve1` och `solve2` (del 1 och del 2 i uppgiften).

```haskell
main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input =
    "<inte löst ännu...>"

solve2 :: String -> String
solve2 input =
    "<inte löst ännu...>"
```

## Initera en ny dag

Skapar en tom källkodsfil för angiven dag och initierar en tom input-fil. 

```bash
./init <dag>
```
**Bonus:** om du skapar en fil `.aoc-session` och lägger in värdet på din sessionskaka från adventofcode.com i denna 
så kommer scriptet även att ladda ned dagens input automatiskt. 

Sessionskakan hittar du enkelt via utvecklar-konsollen i din webbläsare.


## Kör dagens lösning

Kör dagens lösning med scriptet `watch`.

```bash
./watch <dag>
```

Scriptet använder det utmärkta verktyget [entr](https://github.com/eradman/entr) för att automatiskt kompilera om och köra koden vid en förändring. 

Om allt går bra skrivs svaret ut.

```bash
Svar1: 1337
Svar2: <ej löst ännu...>
```

Annars kommer kompilatorns felmeddelanden att visas.  

```bash 
Day0.hs:11:5: error:
    • Variable not in scope: shov :: Int -> String
    • Perhaps you meant ‘show’ (imported from Prelude)
   |
11 |     shov $ length $ concatMap words $ filter (not . isComment) $ lines input
   |     ^^^^
```

Perfekt att köra i ett terminalfönster vid sidan om och få direkt feedback över hur det går.


## Starta repl

Du kan starta ghci med koden för en viss dag med scriptet `repl`

```bash
./repl <dag>

GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( Lib.hs, interpreted )
[2 of 2] Compiling Main             ( Day0.hs, interpreted )
Ok, two modules loaded.
*Main> _
```
