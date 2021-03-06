import System.Directory
import Data.List.Split
import Data.List.Utils
import System.IO

numbers :: [String]
numbers = map show [1..100] 

numbered_categories :: [String] -> [String] -> [String]
numbered_categories [] _ = []
numbered_categories x y= (head y ++ "-" ++head x): numbered_categories (tail x) (tail y)

categories x = do
  all_dir <- getDirectoryContents x
  let d = filter (`notElem` [".", ".."]) all_dir
  return d

cleanInputNumber :: String -> String
cleanInputNumber x = concat (tail (splitOn("-") x))

fileNameFromInput :: [String] -> String -> String
fileNameFromInput xs x = cleanInputNumber( head ( filter (startswith x) xs))

pendingWords :: String -> IO String
pendingWords x = do
  f <- readFile (x ++ "/pending")
  let r = head (lines f)
  length f `seq` return r --  OMG, hack to read the whole File. Lazzy Avaluation?

removePendingFromFile x = do
  f <- readFile(x ++ "/pending") --  OMG, hack to read the whole File. Lazzy Avaluation?
  length f `seq` writeFile (x ++ "/pending") (unlines $ tail $ lines f)

addToFile :: String -> String -> IO()
addToFile x y = do
  appendFile x y

main = do
    let categories_folder = "./data/should_eat"
    -- TODO: Don't display error if empty file
    pw <- pendingWords categories_folder
    c <- categories categories_folder
    let d = numbered_categories c numbers
    putStrLn "::::::::::::::::::::::::"
    putStrLn pw
    putStrLn "::::::::::::::::::::::::"
    mapM_ putStrLn d
    i <- getLine
    let file = fileNameFromInput d i
    addToFile (categories_folder ++ "/" ++ file) ("" ++ pw ++ "\n")
    removePendingFromFile categories_folder
    main
