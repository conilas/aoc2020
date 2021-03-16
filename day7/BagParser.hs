module BagParser where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Map as Map
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import Text.ParserCombinators.ReadP
import Data.Map (Map)
import Data.List

data Bag = Children String | ParentBag String [Bag]

bg = ParentBag "beige" [(Children "blue"), (Children "yellow")]

isVowel :: Char -> Bool
isVowel char =
    any (char ==) "aouei"

vowel :: ReadP Char
vowel =
    satisfy isVowel


dat = readP_to_S vowel "e"
