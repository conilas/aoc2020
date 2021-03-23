module Main where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List
import BagParser
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)

instance Show (a -> b) where
   showsPrec a b = showString (show a ++ show b)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

printParsed [] = print "\n"
printParsed (x:xs) = do
  print x
  printParsed xs

flatten :: [Either ParseError a] -> [a] -> [a]
flatten [] acc = acc
flatten (x:xs) acc = 
  let flattenC = flatten xs in
  case x of 
    Right v ->  flattenC acc ++ [v]
    Left _ -> flattenC acc

reduce :: Either ParseError [Bool] -> Bool
reduce (Left _ ) = False
reduce (Right quorum) = 
  case find (==True) quorum of
    Just _ -> True 
    Nothing -> False

-- serves the purpose of ending the childs and adding a sum operation between them, PERFECT 
-- it is the same as concat in hs but with the special joiner presence
concatJoin :: [[a]] -> a -> [a]
concatJoin xss joiner = [x | xs <- xss, x <- (xs++ [joiner])]

-- instead of calculating directly, we mount an expression and evaluate it later
-- in the future we could comeback and write an expression parser with parsec but
-- now i do not want to 
reduceCount _ [] acc = acc
reduceCount orig (x:xs) acc = 
  case x of 
    ParentBag name children -> concatJoin (map (\x -> reduceCount orig [x] [] ++ acc) children) ")+"
    Children amount name ->
      let f = find (withName name) orig  in 
      case f of 
        Just bg -> accumulate orig bg acc amount
        Nothing -> acc 

accumulate orig bg acc amount =
  let ret = acc ++ [show amount ++ "*"] ++ ["(1"] in
  let z = reduceCount orig [bg] [] in
    case z of
      [] -> ret ++ ["+"]
      other -> ret ++ ["+("] ++ z ++ [")+"]

replace :: String -> String -> String -> String
replace match replace ata = Text.unpack (Text.replace (Text.pack match) (Text.pack replace) (Text.pack ata)) 

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let mapped = map ((regularParse parens) . Text.unpack) ls 
  let flattened = flatten mapped [] 
  let expanded = map (fmap  (\z -> expand z flattened [])) mapped
  let shinny = map (fmap (map (hasShinny))) expanded 
  let expd = expand (Children 0 "shinygold") flattened []
  print (replace "+)" ")" (init (intercalate "" (reduceCount (filterParents expd []) (filterParents expd []) []))))

--output: 131 gg
--output part b: 4*(1+(4*(1)))+3*(1+(1*(1+(4*(1)+5*(1)+3*(1)+2*(1)))+2*(1+(4*(1+(1*(1+(5*(1)+5*(1+(1*(1)+5*(1)+4*(1)))+4*(1)))+1*(1+(4*(1)+3*(1+(1*(1)+5*(1)+4*(1)))+4*(1+(3*(1)))))+5*(1+(2*(1+(3*(1)))+2*(1+(2*(1+(4*(1)))+3*(1)+1*(1+(3*(1)))+5*(1)))+1*(1+(5*(1)+1*(1)))))))+2*(1+(1*(1)+5*(1)+4*(1)))))+2*(1+(5*(1)+5*(1+(1*(1)+5*(1)+4*(1)))+4*(1)))+5*(1+(2*(1+(4*(1)))+3*(1)+1*(1+(3*(1)))+5*(1))))) (so 11261)
