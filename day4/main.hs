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
  let extractV = (==) field . head . splitT ":" in 
    if extractV value then
      let extract = head . tail . splitT ":" in 
        validateFieldFn field (extract value)
      else
        False

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


parseHgt measure value = 
  case Text.unpack measure of 
    "cm" -> 150 <= value && value <= 193 
    "in" -> 50 <= value && value <= 76 
    _ -> False

parseHcl :: [Char] -> Bool
parseHcl [] = True
parseHcl (x:xs) = 
  if present x ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'] then
    parseHcl xs 
  else
    False

interval min max value = 
  let v = read value :: Int in
    min <= v && v <= max

validateFieldFn :: String -> String -> Bool 
validateFieldFn _ value = False 
validateFieldFn "byr" value = interval 1920 2002 value && 4 == length value
validateFieldFn "iyr" value = interval 2010 2020 value && 4 == length value
validateFieldFn "eyr" value = interval 2020 2030 value && 4 == length value

validateFieldFn "hgt" value = 
  let parsed = Text.decimal (Text.pack value) in
    case parsed of
    Right (value, measure)  -> parseHgt measure value 
    _ -> False 

validateFieldFn "hcl" value = 
  let size = ((==) 7 . length) value in
    let headOk = ((==) '#' . head) value in 
      let parsed = parseHcl (tail value) in
        parsed && size && headOk

validateFieldFn "ecl" value = present value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validateFieldFn "pid" value = 
  let size = ((==) 9 . length) value in 
    case Text.decimal (Text.pack value) of
      Left _ -> False
      _ -> size


present value (x:xs) = if x == value then True else present value xs 
present value [] = False

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
