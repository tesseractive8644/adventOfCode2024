import System.IO
import Control.Monad
import Data.List
import Data.Maybe

countXmas :: String -> Int
countXmas [] = 0
countXmas ('X':'M':'A':'S':xs) = succ $ countXmas $ 'S' : xs
countXmas (_:xs) = countXmas xs

rows :: String -> [String]
rows = lines

cols :: String -> [String]
cols = transpose . rows

zipPadding :: [a] -> [[a]] -> [[a]]
zipPadding [] ys = ys
zipPadding (x:xs) (y:ys) = (x:y) : zipPadding xs ys

lDiags' :: [String] -> [[Char]]
lDiags' [br] = singleton <$> br
lDiags' (r:rs) = zipPadding r ([] : lDiags' rs)

lDiags :: String -> [String]
lDiags = lDiags' . rows

rDiags :: String -> [String]
rDiags = lDiags' . reverse <$> rows

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = hGetContents inputStream >>=
    hPrint outputStream . sum . (sum . fmap countXmas <$>) . (liftA2 (++) id (fmap $ fmap $ fmap reverse) [rows, cols, lDiags, rDiags] <*>) . singleton
    -- hPrint outputStream . sum . (sum . fmap countXmas <$>) . (liftA2 (++) id (((reverse <$>) <$>) <$>) [rows, cols, lDiags, rDiags] <*>) . singleton
    -- hPrint outputStream . sum . (sum . fmap countXmas <$>) . ([rows, cols, lDiags, rDiags, fmap reverse <$> rows, fmap reverse <$> cols, fmap reverse <$> lDiags, fmap reverse <$> rDiags] <*>) . singleton

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle