import System.IO
import Control.Monad

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    ln1 <- hGetLine inputStream
    ln2 <- hGetLine inputStream
    let [x,y] :: [Int] = fmap read (words ln1)
    let [x2,y2] :: [Int] = fmap read (words ln2)
    hPrint outputStream (x + y)
    hPrint outputStream (x2 + y2)

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle