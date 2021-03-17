module BagParser where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Map as Map
import Text.Parsec (ParseError, try)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char (oneOf, char, string, digit, satisfy)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import Data.Map (Map)
import Data.List

data Bag = Children String | ParentBag String [Bag] deriving Show

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

children :: Parser Bag
children = do
    void $ many1 whitespaceOrSeparator
    void $ many1 digit
    whitespace
    color1 <- many1 letters 
    whitespace
    color2 <- many1 letters 
    whitespace
    _ <- try (string "bags") <|> string "bag"
    return (Children (color1 ++ color2))
  where
    letters = satisfy isLetter
    whitespaceOrSeparator = satisfy (\x -> x == ' ' || x == ',') 
    

parens :: Parser Bag 
parens = do
    color1 <- many1 letters 
    whitespace
    color2 <- many1 letters 
    whitespace
    void $ string "bags contain"
    childrens <- try (many children) <|> return [] 
    return (ParentBag (color1 ++ color2) childrens)
  where
    letters = satisfy isLetter
