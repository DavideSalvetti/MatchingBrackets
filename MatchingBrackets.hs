import System.IO
import System.Directory

{- Check if the brackets are correctly paired.
   It filters all the brackets from a String, then apply a foldl
   that removes one bracket and the corresponding one until the 
   String is empty. -}
arePaired :: String -> Bool
arePaired xs = null $ foldl removeMatching [] $ filter (`elem` "({[]})") xs

removeMatching :: String -> Char -> String
removeMatching ('(' : xs) ')' = xs
removeMatching ('{' : xs) '}' = xs
removeMatching ('[' : xs) ']' = xs
removeMatching (xs) x         = x:xs

{- Filters only cpp and hpp files. -}
onlycppandhpp :: [String] -> [String]
onlycppandhpp xs = [x | x <- xs, or [(stringContains ".cpp" x), (stringContains ".hpp" x)]]

{- Check if a string contains another string. 
   The first string is the contained string, while 
   the second is the string where we need to search 
   the first one. -}
stringContains :: String -> String -> Bool
stringContains (_:_) [] = False
stringContains xs ys
    | check xs ys = True
    | stringContains xs (tail ys) = True
    | otherwise = False

check :: String -> String -> Bool
check [] _ = True
check (_:_) [] = False
check (x:xs) (y:ys) = (x == y) && check xs ys

{- Returns only the files with correct brackets. -}
getCorrectFiles :: [(String, String)] -> [String]
getCorrectFiles tuples = [a | (a, b) <- tuples, arePaired b]

{- Returns only the files with incorrect brackets. -}
getIncorrectFiles :: [(String, String)] -> [String]
getIncorrectFiles tuples = [a | (a, b) <- tuples, not( arePaired b)]

{- Function to read a file without lazy evaluation. -}
loadFileStrict :: FilePath -> IO String
loadFileStrict f = do
    s <- readFile f
    length s `seq` return s

main = do
	currentDir <- getCurrentDirectory
	putStrLn "--- Analyzing files in:"
	print currentDir
	all <- getDirectoryContents ""
	let filesFound = onlycppandhpp all
	putStrLn "--- Files Found:"
	print filesFound
	contents <- mapM loadFileStrict $ filesFound
	putStrLn "--- Files with correct matching brackets:"
	print(getCorrectFiles $ zip filesFound contents)
	putStrLn "--- Files with incorrect matching brackets:"
	print(getIncorrectFiles $ zip filesFound contents)