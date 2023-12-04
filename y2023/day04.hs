import Text.Parsec.String
import Text.Parsec
import Data.Set ( size, intersection, fromList )
import Data.Either (fromRight)
import Data.Monoid 

number :: Parser Int
number = ( read @Int ) <$> many1 digit

numbers :: Parser [ Int ]
numbers = many1 ( number <* spaces )

data Game = Game Int [ Int ] [ Int ] deriving ( Eq, Show ) 

gameParser :: Parser Game 
gameParser = Game <$> ( string "Card" *> spaces *> number <* string ":" <* spaces )
                  <*> numbers
                  <*> ( string "|" *> spaces *> numbers )

parseInput :: String -> IO ( Either ParseError [ Game ] )
parseInput = parseFromFile ( many gameParser <* eof )

matches :: Game -> Int
matches ( Game num as bs ) = size $ fromList as `intersection` fromList bs

singleCardScore 0 = 0
singleCardScore n = 2 ^ ( n - 1 )

monZap :: Monoid m => [ m ] -> [ m ] -> [ m ] 
monZap [] ys = ys
monZap xs [] = xs
monZap ( x : xs ) ( y : ys ) = ( x <> y ) : monZap xs ys

partB :: [ Int ] -> Int
partB = fst . foldl step ( 0, [] ) 
    where
        pop []         = ( 0, [] )
        pop ( x : xs ) = ( x, xs )
        updateState :: Int -> Int -> [ Int ] -> [ Int ]
        updateState copies cards state = getSum <$> monZap ( replicate cards ( Sum copies ) ) ( Sum <$> state )
        step :: ( Int, [ Int ] ) -> Int -> ( Int, [ Int ] )
        step ( score, state ) matches = let ( bonus, tail ) = pop state
                                            currentCard = 1 + bonus
                                         in ( score + currentCard, updateState currentCard matches tail )
                                        
main :: IO ()
main = do games <- fromRight [] <$> parseInput "data/2023_04.txt" 
          print ( sum ( map ( singleCardScore . matches ) games ) )
          print ( partB ( map matches games ) )
