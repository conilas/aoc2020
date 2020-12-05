import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text

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

walk (x:xs) horizontal acc =
  let current_value = lookupValue (Text.unpack x) 0 horizontal (Text.unpack x) in 
    let next_horiz = horizontal + 3 in
      let eq = fmap (\x -> x == '#') current_value in
        let acc_count = add acc eq in
          walk xs next_horiz acc_count
walk [] x acc = acc 

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let walked = walk ls 0 0
  print walked 
