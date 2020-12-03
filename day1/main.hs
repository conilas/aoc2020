import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

pairUpRec :: [Int] -> Int -> [(Int, Int)] -> [(Int, Int)]
pairUpRec (x:xs) value acc = 
  let pair = (value, x) in 
    pairUpRec xs value (acc ++ [pair])
pairUpRec [] value acc = acc

calc :: [Int] -> [(Int, Int)] -> [(Int, Int)]
calc (x:xs) acc = 
  let value = pairUpRec xs x [] in
    calc xs (acc ++ value) 
calc [] acc = acc

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toInt :: Text.Text -> Int
toInt x = case Text.decimal x of
  Left a -> 0
  Right b -> fst b

sumMultiply :: (Int, Int) -> (Int, Int)
sumMultiply (a, b) = (a + b, a * b)

findSum :: [(Int, Int)] -> Int 
findSum (x:xs) = if fst x == 2020 then snd x else findSum xs 
findSum [] = 0

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let mapped = map toInt ls
  let pairs = calc mapped []
  let aggregated = map sumMultiply pairs
  print (findSum aggregated)
