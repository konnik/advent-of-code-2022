import           Data.Foldable (find)
import           Data.List     (sort)
import           Lib

main :: IO ()
main = runSolver solve1 solve2

solve1 :: String -> String
solve1 input = show $ sum $ filter (<100000) $ dirSize <$> flatten root
    where
        root = fst $ run (emptyDir "/", drop 1 (lines input))

solve2 :: String -> String
solve2 input = show $ find (>=needed) $ sort $ dirSize <$> flatten root
    where
        root = fst $ run (emptyDir "/", drop 1 (lines input))
        needed = 30000000 - unused
        unused = 70000000 - total
        total = dirSize root


type Commands = [String]

data Dir = Dir
    { dirName :: String
    , subDirs ::[Dir]
    , files:: [File]
    } deriving (Show)

data File = File
    { fileName::String
    , fileSize:: Int
    } deriving (Show)


run :: (Dir, Commands) -> (Dir, Commands)
run (dir, []) = (dir, [])
run (cwd, cmd:cmds) =
    case words cmd of
        ["$", "cd", ".."]        -> (cwd, cmds)
        ["$", "cd", subdirName]  -> cd cwd subdirName cmds
        ["$", "ls"]              -> run $ ls cwd cmds
        invalid                  -> error $ "Invalid command: '" ++ unwords invalid ++ "'"

cd :: Dir -> String  -> Commands -> (Dir, Commands)
cd cwd subdirName cmds =
    let
        (subdir, cmds') = run (emptyDir subdirName, cmds)
        dir' = addSubDir cwd subdir
    in
        run (dir', cmds')

ls :: Dir -> Commands -> (Dir, Commands)
ls cwd cmds = (addFiles cwd files, dropWhile ((/='$').head) cmds)
    where
        files = fmap parseFile $ filter ((/="dir").head) $ words <$> takeWhile ((/='$').head) cmds
        parseFile [a,b] = File b (read a)
        parseFile x     = error $ "invalid file: " ++ unwords x


flatten :: Dir -> [Dir]
flatten dir = dir : concatMap flatten (subDirs dir)


emptyDir :: String -> Dir
emptyDir name = Dir name [] []


dirSize :: Dir -> Int
dirSize d = sum (fmap fileSize (files d)) + sum (fmap dirSize (subDirs d))


addSubDir :: Dir -> Dir -> Dir
addSubDir d subdir= d {subDirs = subDirs d ++ [subdir] }

addFiles :: Dir -> [File] -> Dir
addFiles d files = d { files = files }
