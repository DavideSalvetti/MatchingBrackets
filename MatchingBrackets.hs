import System.IO
import System.Directory

arePaired :: String -> Bool
arePaired xs = null $ foldl removeMatching [] $ filter (`elem` "({[]})") xs

removeMatching :: String -> Char -> String
removeMatching ('(' : xs) ')' = xs
removeMatching ('{' : xs) '}' = xs
removeMatching ('[' : xs) ']' = xs
removeMatching (xs) x         = x:xs

onlycppandhpp :: [String] -> [String]
onlycppandhpp xs = [x | x <- xs, stringContains "cpp" x]

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

getCorrectFiles :: [(String, String)] -> [String]
getCorrectFiles tuples = [a | (a, b) <- tuples, arePaired b]

getIncorrectFiles :: [(String, String)] -> [String]
getIncorrectFiles tuples = [a | (a, b) <- tuples, not( arePaired b)]

main = do
		all <- getDirectoryContents ""
		let filesFound = onlycppandhpp all
		putStrLn "Files Found:"
		print filesFound
		contents <- mapM readFile $ filesFound
		putStrLn "Files with correct matching brackets:"
		print(getCorrectFiles( zip filesFound contents))
		putStrLn "Files with incorrect matching brackets:"
		print(getIncorrectFiles ( zip filesFound contents))