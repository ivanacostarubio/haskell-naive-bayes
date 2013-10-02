import System.Directory
import Data.List.Split
import System.IO

numbers :: [String]
numbers = map show [0..100] 

numbered_categories :: [String] -> [String] -> [String]
numbered_categories [] _ = []
numbered_categories x y= (head y ++ "-" ++head x): numbered_categories (tail x) (tail y)

categories x = do
  all_dir <- getDirectoryContents x
  let d = filter (`notElem` [".", ".."]) all_dir
  return d

cleanInputNumber :: String -> String
cleanInputNumber x = concat (tail (splitOn("-") x))

fileNameFromInput :: [String] -> Char -> String
fileNameFromInput xs x = cleanInputNumber( head ( filter (startsWith' x) xs))

startsWith' :: Char -> String -> Bool
startsWith' y x =  (head x) == y

pendingWords :: String -> IO String
pendingWords x = do
  f <- readFile (x ++ "/pending")
  let r = head (lines f)
  print f -- OMG, hack to read the whole File. Lazzy Avaluation?
  return r

removePendingFromFile x = do
  f <- readFile(x ++ "/pending")
  print f -- OMG, hack to read the whole File. Lazzy Avaluation?
  writeFile (x ++ "/pending") (unlines $ tail $ lines f)

addToFile :: String -> String -> IO()
addToFile x y = do
  appendFile x y

main = do
    let categories_folder = "./data/food"
    pw <- pendingWords categories_folder
    c <- categories categories_folder
    let d = numbered_categories c numbers
    mapM_ print d
    print "What category is: "
    print pw
    i <- getLine
    let ii = head i
    let file = fileNameFromInput d ii
    addToFile file ("" ++ pw ++ "\n")
    removePendingFromFile categories_folder
