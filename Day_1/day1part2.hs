import System.IO
import Control.Monad
import Data.List

count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (x:xs) t
 | x == t = 1 + count xs t
 | otherwise = count xs t

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    xs <- hGetContents inputStream
    let as:bs:_ :: [[Int]] = transpose $ fmap read . words <$> lines xs
    hPrint outputStream $ sum $ zipWith (*) as $ count bs <$> as

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle