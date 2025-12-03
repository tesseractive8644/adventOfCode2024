import System.IO
import Control.Monad
import Data.List

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    xs <- hGetContents inputStream
    let as:bs:_ :: [[Int]] = transpose $ fmap read . words <$> lines xs
    hPrint outputStream $ sum $ abs <$> zipWith (-) (sort as) (sort bs)

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle