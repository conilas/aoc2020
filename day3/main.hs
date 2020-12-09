import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

data Slope = S Int Int

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

lookupValue :: [Char] -> Int -> Int -> [Char] -> Maybe Char
lookupValue (x:xs) curr idx all = 
  if curr == idx 
    then (Just x) 
    else lookupValue xs (curr+1) idx all
lookupValue [] curr idx all = lookupValue (all ++ all) curr idx all

add :: Int -> Maybe Bool -> Int
add value (Just True) = value + 1 
add value (Just False) = value 
add value Nothing = value

vertical :: [a] -> Int -> [a]
vertical [] _ = []
vertical (x:xs) 1 = xs 
vertical (x:xs) n = vertical xs (n-1)

walk (x:xs) horizontal acc (S v_walk h_walk) =
  let current_value = lookupValue (Text.unpack x) 0 horizontal (Text.unpack x) in 
    let next_horiz = horizontal + h_walk  in
      let vertical_rest = vertical (x:xs) v_walk in
      let eq = fmap (\x -> x == '#') current_value in
        let acc_count = add acc eq in
          walk vertical_rest next_horiz acc_count (S v_walk h_walk) 
walk [] x acc _ = acc 

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let partial_walkable = walk ls 0 0
  let slope = [(S 1 1), (S 1 3), (S 1 5), (S 1 7), (S 2 1)]
  let final = (foldr (*) 1) . map partial_walkable 
  print (final slope)
