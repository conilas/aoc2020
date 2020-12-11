import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List

singleValue :: (Char, Int) -> Int
singleValue ('F', _) = 0 
singleValue ('B', idx) = 2 ^ idx
singleValue ('L', _) = 0 
singleValue ('R', idx) = 2 ^ idx
singleValue (_, _) = 0

extractValue :: [(Char, Int)] -> Int 
extractValue = foldr (+) 0 . map singleValue 

append :: [a] -> a -> [a]
append ls v = ls ++ [v]

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 ls = ls
remove amount (x:xs) = ((\z -> remove z xs) . pred) amount

zipIndex :: [a] -> [(a, Int)]
zipIndex [] = []
zipIndex ls = zip ls [0..]

toBinary :: [Char] -> [(Char, Int)]
toBinary = zipIndex . remove 3 . reverse

toBinaryCol :: [Char] -> [(Char, Int)]
toBinaryCol = zipIndex . reverse . remove 7 

aggregate :: (Int, Int) -> Int
aggregate (row, col) = row * 8 + col 

maxV :: [Int] -> Int -> Int
maxV [] acc = acc
maxV (x:xs) acc = if x > acc then maxV xs x else maxV xs acc

lookAhead :: [Int] -> Int
lookAhead (x:y:xs) = if (succ . succ) x == y then succ x else lookAhead (y:xs)

toValue fn = extractValue . fn . Text.unpack

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let row = map (toValue toBinary) ls in 
    let col = map (toValue toBinaryCol) ls in
      let final = map aggregate (zip row col) in
        print (map (\z -> z final) [(\x -> maxV final 0) . sort, lookAhead . sort])
          
        
  

