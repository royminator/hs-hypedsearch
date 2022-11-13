module Main (main) where

import Bio.Data.Fasta
import Bio.Seq
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    (s:_) <- readFasta' "/home/roy/data/genome/vibrio_cholerae_genome" :: IO [DNA IUPAC]
    printSeq s

printSeq :: DNA IUPAC -> IO ()
printSeq s = do
    let a = toBS . convert $ s
    B.putStrLn (B.filter (\c -> B.notElem c (B.pack "ACTG")) a)
    putStrLn ("len: " ++ show (Bio.Seq.length s))
    B.putStrLn a

convert :: DNA alphabet -> DNA Basic
convert = unsafeFromBS . toBS
