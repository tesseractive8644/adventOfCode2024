import System.IO
import Control.Monad
import Data.List
import Data.Maybe

-- Prec: three lists of same length
triZip :: [a] -> [a] -> [a] -> [[a]]
triZip [] [] [] = []
triZip (x:xs) (y:ys) (z:zs) = (x:y:[z]) : triZip xs ys zs

mapTriZip :: [[a]] -> [[[a]]]
mapTriZip (xs:xss@(ys:zs:_)) = triZip xs ys zs : mapTriZip xss
mapTriZip _ = []

-- Prec: 3x3 list of char
checkCrossMas :: [[Char]] -> Bool
checkCrossMas [[tl,_,tr],[_,c,_],[bl,_,br]] = c == 'A' && 
                                              ((tl == 'M' && br == 'S') || (tl == 'S' && br == 'M')) &&
                                              ((tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M'))

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = hGetContents inputStream >>=
    hPrint outputStream . sum . (sum . fmap (fromEnum . checkCrossMas) <$>) . mapTriZip . transpose . mapTriZip . lines

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle