import Text.Parsec
import Text.Parsec.String
import Control.Applicative ( liftA3 )
import Data.Either
import Data.Maybe
import Control.Monad
import Aoc
import qualified Control.Applicative as A

data Interval = Interval Int Int Int deriving ( Show, Eq )
newtype Map = Map [ Interval ] deriving ( Show, Eq )

intervalParser :: Parser Interval
intervalParser = liftA3 Interval ( number <* spaces ) ( number <* spaces ) ( number <* spaces )

mapParser :: Parser Map
mapParser =  Map <$> ( nonspaces *> spaces *> nonspaces *> spaces *> many1 intervalParser )


inputParser :: Parser ( [ Int ], [ Map ] )
inputParser = (,) <$> ( string "seeds:" *> spaces *> numbers )
                  <*> many mapParser

funOfInterval :: Interval -> Int -> Maybe Int
funOfInterval ( Interval dest src amount ) val = if val >= src && val - src < amount 
                                                    then Just ( dest + ( val - src ) )
                                                    else Nothing

--f `comb` ( g `comb` ( h `comb` [] ) )

applyFirst :: [ a -> Maybe b ] -> a -> Maybe b
applyFirst funs x = foldr ( \f -> (A.<|>) $ f x ) Nothing funs

funOfMap :: Map -> Int -> Int 
funOfMap ( Map intervals ) x = fromMaybe x $ applyFirst ( map funOfInterval intervals ) x

(|>) = flip (.)

evalMaps :: [ Map ] -> Int -> Int
evalMaps = foldr ((|>) . funOfMap ) id 

interSingleton :: Int -> Int -> [ ( Int, Int ) ]
interSingleton a b = [ ( a, b ) | a < b ]

safeHead :: [ a ] -> Maybe a
safeHead [] = Nothing
safeHead ( x : xs ) = Just x

interSection :: ( Int, Int ) -> ( Int, Int ) -> Maybe ( Int, Int ) 
interSection ( amin, amax ) ( bmin, bmax ) = safeHead ( interSingleton ( min amin bmin ) ( max amax bmax ) )

interSize :: Maybe ( Int, Int ) -> Int
interSize Nothing = 0
interSize ( Just ( a, b ) ) = b - a

intervalFunOfInteval :: Interval -> ( Int, Int ) -> [ ( Int, Int ) ]
intervalFunOfInteval ( Interval dest src amount ) ( start, end ) = 
    let size = ( dest + interSize ( interSection ( src, src + amount ) ( start, end ) ) )
     in interSingleton start src 
        ++ interSingleton dest ( dest + size )
        ++ interSingleton ( start + size ) end

intervalFunOfMap :: Map -> ( Int, Int ) -> [ ( Int, Int ) ]
intervalFunOfMap ( Map intervals ) interval = 
    foldl ( flip concatMap ) 
          [ interval ] 
          ( map intervalFunOfInteval intervals )

-- todo: do the pairs and then just run the above

main :: IO ()
main = do ( seeds, maps ) <- fromRight ([], []) <$> parseFromFile inputParser "data/2023_05.txt"
          print $ minimum ( map ( evalMaps maps ) seeds )
