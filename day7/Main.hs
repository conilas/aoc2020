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

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let mapped = map ((regularParse parens) . Text.unpack) ls 
  printParsed mapped
--  print (regularParse parens "plaid beige bags contain 3 drab magenta bags, 4 dull indigo bags.")
--  print (regularParse parens "plaid beige bags contain 3 drab magenta bags.")
