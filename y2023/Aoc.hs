module Aoc where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

number :: Parser Int
number = ( read @Int ) <$> many1 digit

numbers :: Parser [ Int ]
numbers = many1 ( number <* spaces )

nonspaces :: Parser String
nonspaces = many1 ( noneOf [ ' ', '\t', '\r', '\n' ] )
