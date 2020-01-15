-- https://codeforces.com/contest/1265/problem/A
-- 3
-- a???cb => ababcb
-- a??bbc => -1
-- a?b?c => acbac


main :: IO ()
main = do
  input <- getLine
  let linesToRead = read input
  tryDecorateString linesToRead
  
tryDecorateString :: Int -> IO ()
tryDecorateString 0 = pure ()
tryDecorateString n = do
  currentLine <- getLine
  if isStringBeautiful currentLine Nothing
  then putStrLn (reverse (decorateString currentLine []))
  else putStrLn "-1"
  tryDecorateString (n - 1)
  
alphabet :: [Char]
alphabet = ['a', 'b', 'c']
  
-- check that string doesn't contains any double characters
isStringBeautiful :: [Char] -> Maybe Char -> Bool
isStringBeautiful (x:xs) Nothing =
  if (length xs) == 0
  then True
  else isStringBeautiful xs (Just x)
  
isStringBeautiful [] _ = True
isStringBeautiful (x:xs) (Just prevChar) =
  if x == prevChar && x /= '?'
  then False
  else isStringBeautiful xs (Just x)

decorateString :: [Char] -> [Char] -> [Char]
decorateString word formattedWord =
  case word of
  (x:[]) -> case x of
    '?' -> case formattedWord of
      -- start iteration
      [] -> [anyChar] 
      -- end of iteration
      (r:rs) -> allowedEnd:r:rs
    _ -> x:formattedWord
  (x:xs) -> case x of
    '?' -> case formattedWord of
      [] -> decorateString xs (allowedStart:[])
      (r:rs) -> decorateString xs (allowedMiddle:r:rs)
    _   -> decorateString xs (x:formattedWord)
  [] -> []
  where
    anyChar = alphabet !! 0
    nextChar = word !! 1
    prevChar = head formattedWord
    allowedStart = (filter (/= nextChar) alphabet) !! 0
    allowedMiddle = (filter (\x -> x /= prevChar && x /= nextChar) alphabet) !! 0
    allowedEnd = (filter (/= prevChar) alphabet) !! 0
 
  
tests :: [String]
tests = ["a???cb", "a??bbc", "a?b?c", "a??a"]