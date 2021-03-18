module Main where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List
import BagParser
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

printParsed [] = print "\n"
printParsed (x:xs) = do
  print x
  printParsed xs

flatten :: [Either ParseError a] -> [a] -> [a]
flatten [] acc = acc
flatten (x:xs) acc = 
  case x of 
    Right v -> flatten xs acc ++ [v]
    Left _ -> flatten xs acc

reduce :: Either ParseError [Bool] -> Bool
reduce (Right quorum) = 
  case find (==True) quorum of
    Just _ -> True 
    Nothing -> False
reduce (Left _ ) = False

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let mapped = map ((regularParse parens) . Text.unpack) ls 
  let flattened = flatten mapped [] 
-- magic: recursevily expand the bags and their children, accumulate it all together, find the ones that reach shinny, count 
  let expanded = map (fmap  (\z -> expand z flattened [])) mapped
  let shinny = map (fmap (map (hasShinny))) expanded 
  print (length (filter (==True) (map reduce shinny)))

--output: 131 gg
