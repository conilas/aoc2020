import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

space :: String
space = " "

empty :: String
empty = ""

doT :: (Text.Text -> Text.Text) -> String -> String
doT f = Text.unpack . f . Text.pack 

stripT :: String -> String
stripT = doT Text.strip 

splitT :: String -> String -> [String]
splitT splitter lines = 
  let fn = (map Text.unpack) . Text.splitOn (Text.pack splitter) . Text.pack in
    fn lines 

accumulatePassport :: [String] -> String -> [String] -> [String]
accumulatePassport [] curr acc = (acc ++ [curr])
accumulatePassport (x:xs) curr acc = 
  if x == empty then 
    accumulatePassport xs empty (acc ++ [stripT curr])
  else 
    accumulatePassport xs (curr ++ space ++ x) acc

parseLine :: String -> [String]
parseLine line = splitT space line

validateField :: String -> String -> Bool
validateField value field = 
  let eqV = (==) field . head . splitT ":" in 
    eqV value

-- validate line and return the next number decisor 
-- fucc booleans
validateLine :: [String] -> [String] -> [String] -> (Int -> Int)
validateLine _ _ [] = succ 
validateLine _ [] _ = id 
validateLine fields (y:ys) (x:xs) =
  if validateField y x then
    validateLine fields fields xs 
  else 
    validateLine fields ys (x:xs)

validate :: [[String]] -> Int -> Int
validate (x:xs) acc = 
  let validation = validateLine x x ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] in
    validate xs (validation acc)
validate [] acc = acc

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let textPasses = map Text.unpack ls
  let passes = accumulatePassport textPasses empty []
  let parsed = (\x -> validate x 0) . map parseLine 
  print (parsed passes)
