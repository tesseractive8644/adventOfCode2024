import System.IO
import Control.Monad

count :: [Bool] -> Int
count [] = 0
count (True:xs) = 1 + count xs
count (False:xs) = count xs

allPos :: [Int] -> Bool
allPos = all $ (== 1) . signum

allNeg :: [Int] -> Bool
allNeg = all $ (== (-1)) . signum

allLEQ3 :: [Int] -> Bool
allLEQ3 = all $ (<=3) . abs

safeLevel :: [Int] -> Bool
safeLevel = liftA2 (&&) allLEQ3 $ liftA2 (||) allPos allNeg

diffs :: [Int] -> [Int]
diffs = liftA2 (zipWith (-)) id tail

tryLevels :: [Int] -> [Int] -> [Bool]
tryLevels ys [] = [safeLevel ys]
tryLevels ys (x:xs) = safeLevel (diffs $ reverse ys ++ xs) : tryLevels (x:ys) xs

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = hGetContents inputStream >>= 
    (hPrint outputStream . count . ((or . tryLevels [] . fmap read . words) <$>) . lines)

-- liftA2 :: (Bool -> Bool -> Bool) -> ([Int] -> Bool) -> ([Int] -> Bool) -> ([Int] -> Bool)

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle