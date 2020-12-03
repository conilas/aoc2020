import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

data Rule = R ((Int,Int), Char) 
  deriving Show

extractSnd :: Rule -> Char 
extractSnd (R (_, b)) = b

extractFst :: Rule -> (Int, Int)
extractFst (R (a, _)) = a

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- if we had dep types this could be avoided 
toPair :: [Text.Text] -> Maybe (Text.Text, Text.Text)
toPair (x:y:[]) = Just (x, y)
toPair _ = Nothing 

toInt :: Text.Text -> Int
toInt x = case Text.decimal x of
  Left a -> 0
  Right b -> fst b

fstChar :: Text.Text -> Char
fstChar = head . Text.unpack . Text.strip

textPairToRule :: (Text.Text, Text.Text) -> Maybe ((Int,Int), Char)
textPairToRule (x, y) = 
  let valueRange = (ruleCommands "-" x) in
    case toPair valueRange of
      Just (min, max) -> Just ((toInt min, toInt max), fstChar y)
      Nothing -> Nothing

parseSingleRule :: Text.Text -> Maybe Rule 
parseSingleRule text = 
  let parsedRule = Text.splitOn (Text.pack " ") text in
    let z = (>>=) (toPair parsedRule) textPairToRule in 
      fmap R z

ruleCommands :: [Char] -> Text.Text -> [Text.Text]
ruleCommands split value = Text.splitOn (Text.pack split) value

toRulePair :: Maybe (Text.Text, Text.Text) -> Maybe (Rule, Text.Text)
toRulePair Nothing = Nothing
toRulePair (Just (x, y)) = 
  let value = parseSingleRule x in
    case value of 
      Just a -> Just (a, Text.strip y)
      Nothing -> Nothing

toRule :: Text.Text -> Maybe (Rule, Text.Text) 
toRule = toRulePair . toPair . (ruleCommands ":")

countValueRec :: [Char] -> Char -> Int -> Int 
countValueRec (x:xs) r amount = if x == r then countValueRec xs r (amount + 1) else countValueRec xs r amount
countValueRec [] r amount = amount


applyRule :: (Rule, Text.Text) -> Bool 
applyRule (rule, password) = 
  let unpacked = Text.unpack password in
    let countedTimes = countValueRec unpacked (extractSnd rule) 0 in 
      let constraints = extractFst rule in
        (fst constraints) <= countedTimes && countedTimes <=  (snd constraints) 

checkCondition :: Maybe Bool -> Bool
checkCondition (Just a) = a
checkCondition Nothing = False 

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let mapped = map toRule ls
  let applied = map (fmap applyRule) mapped
  let final = [i | i <- applied, checkCondition i]
  print (length final)
