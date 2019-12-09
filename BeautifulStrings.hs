import Control.Monad

main :: IO ()
main = do
  quantity <- getLine
  let linesToRead = read quantity
  testCases <- replicateM linesToRead getLine
  mapM_ (putStrLn .  tryDecorateString) testCases

tryDecorateString :: [Char] -> [Char]
tryDecorateString input = 
  if isStringBeautiful input Nothing
  then decorateString Nothing input
  else "-1"

alphabet :: [Char]
alphabet = ['a', 'b', 'c']

-- check that string doesn't contains any double characters
isStringBeautiful :: [Char] -> Maybe Char -> Bool
isStringBeautiful (x:xs) Nothing =
  if (length xs) == 0
  then True
  else isStringBeautiful xs (Just x)

isStringBeautiful (x:xs) (Just prevChar) =
  if x == prevChar && x /= '?'
  then False
  else isStringBeautiful xs (Just x)

isStringBeautiful [] _ = True

-- beautify string
decorateString :: Maybe [Char] -> [Char] -> [Char]
decorateString Nothing (currentChar:otherChars)
  | length otherChars == 0 && currentChar == '?' = [anyChar]
  | length otherChars == 0 && currentChar /= '?' = [currentChar]
  | length otherChars > 0  && currentChar == '?' = decorateString (Just [allowedChar]) otherChars 
  | length otherChars > 0  && currentChar /= '?' = decorateString (Just [currentChar]) otherChars 
  where
    anyChar = alphabet !! 0
    nextChar = head otherChars
    allowedChar = (filter (/= nextChar) alphabet) !! 0

decorateString (Just formatedString) (currentChar:[])
  | currentChar == '?' = formatedString ++ [allowedChar]
  | otherwise = formatedString ++ [currentChar]
  where
    previousChar = last formatedString
    allowedChar = (filter (/= previousChar) alphabet) !! 0

decorateString (Just formatedString) (currentChar:otherChars)
  | currentChar == '?' = decorateString (Just (formatedString ++ [allowedChar])) otherChars
  | currentChar /= '?' = decorateString (Just (formatedString ++ [currentChar])) otherChars
  where
    nextChar = head otherChars
    previousChar = last formatedString
    allowedChar = (filter (\char -> char /= previousChar && char /= nextChar) alphabet) !! 0


tests :: [String]
tests = ["a???cb", "a??bbc", "a?b?c", "a??a"]