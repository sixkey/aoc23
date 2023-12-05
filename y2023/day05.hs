import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Applicative ( liftA3 )
import Data.Either

data Interval = Interval Int Int Int deriving ( Show, Eq )
newtype Map = Map [ Interval ] deriving ( Show, Eq )

number :: Parser Int
number = ( read @Int ) <$> many1 digit

numbers :: Parser [ Int ]
numbers = many1 ( number <* spaces )

intervalParser :: Parser Interval
intervalParser = liftA3 Interval ( number <* spaces ) ( number <* spaces ) ( number <* spaces )

mapParser :: Parser Map
mapParser =  Map <$> ( nonspaces *> spaces *> nonspaces *> spaces *> many1 intervalParser )

nonspaces :: Parser String
nonspaces = many1 ( noneOf [ ' ', '\t', '\r', '\n' ] )

inputParser :: Parser ( [ Int ], [ Map ] )
inputParser = (,) <$> ( string "seeds:" *> spaces *> numbers )
                  <*> many mapParser

main :: IO ()
main = do ( seeds, maps ) <- fromRight ([], []) <$> parseFromFile inputParser "data/2023_05_test.txt"
          print ( seeds, maps )
