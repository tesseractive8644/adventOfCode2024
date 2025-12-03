import System.IO
import Control.Monad

count :: [Bool] -> Int
count [] = 0
count (True:xs) = 1 + count xs
count (False:xs) = count xs

allPos :: [Int] -> Bool
allPos = all ((== 1) . signum)

allNeg :: [Int] -> Bool
allNeg = all ((== (-1)) . signum)

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let reps :: [[Int]] = fmap read . words <$> lines doc
    let diffs = fmap (\xs -> zipWith (-) xs (tail xs)) reps
    let incrDecr = liftA2 (||) allPos allNeg <$> diffs
    hPrint outputStream $ count $ zipWith (&&) (all ((<=3) . abs) <$> diffs) incrDecr

-- liftA2 :: (Bool -> Bool -> Bool) -> ([Int] -> Bool) -> ([Int] -> Bool) -> ([Int] -> Bool)

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle