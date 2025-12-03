import System.IO
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Char
import Data.List

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser pa) = Parser $ fmap (first f) . pa

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> [(x,s)]

    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f (Parser pa) (Parser pb) = Parser $ \s -> [(f a b, s'') | (a, s') <- pa s, (b, s'') <- pb s']

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const []

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser pa <|> Parser pb = Parser $ \s -> pa s ++ pb s

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser eat
 where
    eat :: String -> [(Char, String)]
    eat (c:cs)
     | f c = [(c,cs)]
    eat _ = []

digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

parseAll :: Parser String -> String -> Bool -> [(String, String)]
parseAll _ "" _ = []
parseAll p str@(_:tr) fg 
 | not $ null $ parse (string "don't()") str =  parseAll p tr False
 | not $ null $ parse (string "do()") str =  parseAll p tr True
 | not fg = parseAll p tr fg
 | otherwise = parse p str ++ parseAll p tr fg

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let xs :: [Int] = read . fst <$> parseAll (string "mul(" *> some digit <* string "," <* some digit <* string ")") doc True
    let ys :: [Int] = read . fst <$> parseAll (string "mul(" *> some digit *> string "," *> some digit <* string ")") doc True
    hPrint outputStream $ sum $ zipWith (*) xs ys

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle